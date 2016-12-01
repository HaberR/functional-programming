open Lwt
open Type_info

type user_info = {
  username: id; 
  mutable blocked: string list; 
  mutable rooms: chatroom list;
  oc: Lwt_io.output_channel;
}

let (clients : (id, user_info) Hashtbl.t) = Hashtbl.create 100
let (rooms : (string, chatroom * msg list) Hashtbl.t) = Hashtbl.create 100
let (games : (string, gameroom * square list) Hashtbl.t) = 
  Hashtbl.create 100 

(***************************************************)
(****** handlers and helpers for handle_request ****)
(***************************************************)

(* [handle_login i oc] returns a success response
 * and registers the user if they aren't already registered
 * (adding them to the client hashtbl) *)
let handle_login i out_chan =
  if Hashtbl.mem clients i |> not then
    let info = { 
      username = i;
      blocked = [];
      rooms = [];
      oc = out_chan
    } in
    Hashtbl.add clients i info;
    (Nothing, Success)
  else (Nothing, Success)
  
(*[common_elem l1 l2] is l3 where x is in l3 iff
 * x is in l1 and x is in l2 *)
let common_elem lst1 lst2 =
  let mem x = List.mem x lst2 in
  lst1 |> List.filter mem

(* [check_room cr] is a list of tuples (x,y) where 
 * (x,y) is in the list iff y is blocked by x
 * Raises Not_found if a participant is not actually 
 * a registered user *)
let check_room cr =
  let fold lst member =
    let user = Hashtbl.find clients member in
    let blocked = 
      common_elem cr.participants user.blocked in
    let stringified = 
      blocked |> List.map 
      (fun x -> user.username ^ " has blocked " ^ x) in
    stringified @ lst in
  List.fold_left fold [] cr.participants

let check_game gr = 
  let fold lst member =
    let user = Hashtbl.find clients member in
    let blocked = 
      common_elem gr.players user.blocked in
    let stringified = 
      blocked |> List.map 
      (fun x -> user.username ^ " has blocked " ^ x) in
    stringified @ lst in
  List.fold_left fold [] gr.players

let list_to_string lst =
  let fold joined s = joined ^ "\n" ^ s in
  List.fold_left fold "" lst

(* [leave u t] goes through all the chatrooms
 * that u has in common with [t] and removes [u]
 * from them. Raises Not_found if [t] is not registered *)
let leave u t =
  let target = Hashtbl.find clients t in
  let replace {name = nm; participants = p} =
    let m_hst = Hashtbl.find rooms nm |> snd in
    let p' = p |> List.filter ((<>) u) in
    let rm' = ({ name = nm; participants = p' }, m_hst) in
    Hashtbl.replace rooms nm rm' in
  target.rooms |> List.iter replace


(* [handle_block u t] removes [u] from all the
 * chat rooms [t] is in and adds [t] to [u]'s
 * block list *)
let handle_block u t =
  if Hashtbl.mem clients u then
    if Hashtbl.mem clients t then
      let user_info = Hashtbl.find clients u in
      user_info.blocked <- t :: user_info.blocked;
      leave u t;
      (Nothing, Success)
    else (Nothing, Fail "target is not registered")
  else (Nothing, Fail "user is not registered")

(* [post_message msg] posts a message to the room
 * specified in [msg] provided that the sender id
 * has access to the room and the room exists. *)
let post_message msg = 
  let rmname = msg.room.name in
  if Hashtbl.mem rooms rmname then
    let (rm, msgs) = Hashtbl.find rooms rmname in
    if rm.participants |> (List.mem msg.user) then
      let msgs' = msg :: msgs in
      Hashtbl.replace rooms rm.name (rm, msgs');
      (Nothing, Success)
    else (Nothing, Fail "You don't have access to that room")
  else (Nothing, Fail ("No such room exists: " ^ rmname))

(* [post_room cr] creates a new chat room provided
 * that the chat room name does not exist already
 * and none of the members blocked each other *)
let post_room (cr : chatroom) = 
  if Hashtbl.mem rooms cr.name |> not then
    try 
      let blocked = check_room cr in
      if List.length blocked = 0 then
        (Hashtbl.add rooms cr.name (cr, []); 
        (Nothing, Success))
      else 
        let err_msg = blocked |> list_to_string in
        (Nothing, Fail err_msg)
    with
    | Not_found -> 
        (Nothing, Fail "one or more participants is not registered")
  else (Nothing, Fail "room name exists already")

let post_game gr = 
  if Hashtbl.mem games gr.name |> not then 
    try 
      let blocked = check_game gr in 
      if List.length blocked = 0 then 
        (Hashtbl.add games gr.name (gr, [N;N;N;N;N;N;N;N;N]) ; 
        (Nothing, Success))
      else 
        let err_msg = blocked |> list_to_string in 
        (Nothing, Fail err_msg)
    with
    | Not_found -> (Nothing, Fail "one or more players is not registered")
  else (Nothing, Fail "game name exists already")

let get_games id = 
  let fold _ (gr,_) lst = 
    if List.mem id gr.players then gr :: lst
    else lst in 
  let lst = Hashtbl.fold fold games [] in 
  (Gamerooms lst, Success) 

let get_rooms i =
  let fold _ (cr,_) lst =
    if List.mem i cr.participants then cr :: lst
    else lst in
  let lst = Hashtbl.fold fold rooms [] in
  (Chatrooms lst, Success)

let get_messages uname cr =
  if List.mem uname cr.participants then
    if Hashtbl.mem rooms cr.name then
      let msgs = Hashtbl.find rooms cr.name |> snd in
      (Messages msgs, Success)
    else (Nothing, Fail "no such room exists")
  else (Nothing, Fail "you don't have access to that room")

let get_room i crname =
  if Hashtbl.mem rooms crname then
    let cr = Hashtbl.find rooms crname |> fst in
    if List.mem i cr.participants then
      (Chatroom cr, Success)
    else (Nothing, Fail "You don't have access to that room")
  else (Nothing, Fail "That room doesn't exist")

let get_users () =
  let fold i _ lst = i :: lst in
  let lst = Hashtbl.fold fold clients [] in
  (Users lst, Success)

let handle_get_game id grname =
  if Hashtbl.mem games grname then 
    let gr, st = Hashtbl.find games grname in 
    if List.mem id gr.players then 
      (Gamestate (gr,st), Success)
    else (Nothing, Fail "invalid id for this game")
  else (Nothing, Fail "invalid game")

(***************************************************)
(****** end of handlers and helpers *****************)
(***************************************************)
let handle_request req oc =
  match req |> req_from_string with
  | Login identifier -> handle_login identifier oc
  | Block (user, target) -> handle_block user target
  | Message msg -> post_message msg
  | Listrooms identifier -> get_rooms identifier
  | Listmessages (identifier, cr) -> get_messages identifier cr
  | Newroom cr -> post_room cr
  | Getroom (identifier, crname) -> get_room identifier crname
  | Listusers -> get_users ()
  | Newgame gr -> post_game gr
  | Listgames id -> get_games id 
  | Getgame (id, grname) -> handle_get_game id grname 

let send_response oc resp = 
  resp 
  |> Type_info.resp_to_string 
  |> Lwt_io.write_line oc


  (* [handle_connection ic oc] sets up a listening system for 
   * ic which will call handle_request on any lines which are
   * read *)
let rec handle_connection ic oc () =
  Lwt_io.read_line_opt ic >>= function
  | Some ln -> 
      handle_request ln oc |> 
      send_response oc >>= 
      handle_connection ic oc
  | None -> Lwt_log.info "Connection closed!" >>= return

(* creates the ic and oc from the connection,
 * and passes them onto handle_connection. Also
 * handles failure *)
let accept_connection conn =
  (* conn is (fd * sock_addr) pair *)
  let fd, _ = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt.on_failure (handle_connection ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt_log.info "New connection" >>= return

let backlog = 10

(* creates a server which is continuously accepting connections
 * and handling them with accept_connection*)
let create_server sock =
  let rec serve () =
    Lwt_unix.accept sock >>= accept_connection >>= serve
  in serve

(* assume this just creates the socket ...*)
let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock @@ ADDR_INET(Unix.inet_addr_loopback, 3110);
  listen sock backlog;
  sock

let server =
  let soc = create_socket () in
  create_server soc

let () =
  Lwt_main.run (server ())

