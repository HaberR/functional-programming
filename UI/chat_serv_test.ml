open Lwt
open Type_info

type user_info = {
  username: id; 
  password: id; 
  mutable wl: (int*int);
  mutable blocked: string list;
}

type game_info = {
  gr: gameroom ; 
  mutable board: square list ;
  mutable last_turn: id option 
}

let (clients : (id, user_info) Hashtbl.t) = Hashtbl.create 100
let (rooms : (string, chatroom * msg list) Hashtbl.t) = Hashtbl.create 100

(*Hashtable mapping names of games to their game_infos*)
let (games : (string, game_info) Hashtbl.t) = Hashtbl.create 100 


(***************************************************)
(****** handlers and helpers for handle_request ****)
(***************************************************)

let (|>?) u f =
  if Hashtbl.mem clients u then f u
  else (Nothing, Fail (u ^ " is not registered"))

let (|>??) r f = 
  if Hashtbl.mem rooms r then f r
  else (Nothing, Fail ("the room " ^ r ^ " does not exist"))

(* [handle_login i oc] returns a success response
 * and registers the user if they aren't already registered
 * (adding them to the client hashtbl) *)
let handle_login i oc =
  i |>? fun _ -> (Nothing, Success)
  (*if Hashtbl.mem clients i then
    (Nothing, Success)
  else (Nothing, Fail "user not registered")*)

let handle_reg id pswd oc = 
  let info = { 
      username = id;
      password = pswd;
      wl = (0,0);
      blocked = [];(* 
      rooms = [];
      oc = oc *)
    } in
    Hashtbl.add clients id info;
    (Nothing, Success)

let handle_auth u pswd oc = 
  u |>? fun nm ->
  let target = Hashtbl.find clients nm in 
  if target.password=pswd then (Nothing, Success)
  else (Nothing, Fail "wrong password")

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

(*[check_game gr] is a list of tuples (x,y) where
 *(x,y) is in the list iff y is blocked by x
 *Raises Not_found if a participant is not a registered
 *user*)
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

let unpackaged_get_rooms i =
  let fold _ (cr,_) lst =
    if List.mem i cr.participants then cr :: lst
    else lst in
  Hashtbl.fold fold rooms []

let get_rooms i =
  let lst = unpackaged_get_rooms i in
  (Chatrooms lst, Success)

(* [leave u t] goes through all the chatrooms
 * that u has in common with [t] and removes [u]
 * from them. Raises Not_found if [t] is not registered *)
let leave u t =
  let target = Hashtbl.find clients t in
  let replace ({name = nm; participants = p} as cr) =
    let m_hst = Hashtbl.find rooms nm |> snd in
    let p' = p |> List.filter ((<>) u) in
    let rm' = ({ cr with participants = p'}, m_hst) in
    Hashtbl.replace rooms nm rm' in
  let rms = unpackaged_get_rooms t  in
  rms |> List.iter replace


(* [handle_block u t] removes [u] from all the
 * chat rooms [t] is in and adds [t] to [u]'s
 * block list *)
let handle_block user target =
  user |>? fun u ->
  target |>? fun t ->
  let user_info = Hashtbl.find clients u in
  user_info.blocked <- t :: user_info.blocked;
  leave u t;
  (Nothing, Success)

let handle_unblock user target =
  user |>? fun u ->
  target |>? fun t ->
  let u_info = Hashtbl.find clients u in
  let blocked' = u_info.blocked |> List.filter ((<>) t) in
  u_info.blocked <- blocked'; 
  (Nothing, Success)


(* [post_message msg] posts a message to the room
 * specified in [msg] provided that the sender id
 * has access to the room and the room exists. *)
let post_message msg = 
  msg.room.name |>?? fun rmname ->
  let (rm, msgs) = Hashtbl.find rooms rmname in
  if rm.participants |> (List.mem msg.user) then
    let msgs' = msg :: msgs in
    Hashtbl.replace rooms rm.name (rm, msgs');
    (Nothing, Success)
  else (Nothing, Fail "You don't have access to that room")

(* [post_room cr] creates a new chat room provided
 * that the chat room name does not exist already
 * and none of the members blocked each other *)
let post_room (cr : chatroom) = 
  if Hashtbl.mem rooms cr.name |> not then
    try 
      let blocked = check_room cr in
      if List.length blocked = 0 then
        let p' = cr.participants |> List.sort_uniq compare in
        let cr' = {cr with participants = p'} in
        (Hashtbl.add rooms cr.name (cr', []); 
        (Nothing, Success))
      else 
        let err_msg = blocked |> list_to_string in
        (Nothing, Fail err_msg)
    with
    | Not_found -> 
        (Nothing, Fail "one or more participants is not registered")
  else (Nothing, Fail "room name exists already")

(*[post_game gr] creates a new game room specified by [gr] as long
 *as the desired name is not already a game and both players are registered*)
let post_game gr = 
  if Hashtbl.mem games gr.name |> not then 
    try 
      let blocked = check_game gr in 
      if List.length blocked = 0 then 
        (Hashtbl.add games gr.name {gr=gr;board=[N;N;N;N;N;N;N;N;N];last_turn=None} ; 
        (Nothing, Success))
      else 
        let err_msg = blocked |> list_to_string in 
        (Nothing, Fail err_msg)
    with
    | Not_found -> (Nothing, Fail "one or more players is not registered")
  else (Nothing, Fail "game name exists already")

(*[get_games id] returns a list of all the games that the user 
 *specified by [id] is a part of*)
let get_games id = 
  let fold _ {gr=gr;board=_;last_turn=_} lst = 
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

(*returns the reversed list of all messages
 * more recent than m in mlst. Requires
 * that mlst be sorted such that most recent
 * messages are first *)
let shorten_messages m mlst =
  let rec get_lst lst acc m =
    match lst with
    | h :: t -> 
        if h = m then acc 
        else get_lst t (h :: acc) m
    | [] -> [] in
  match m,mlst with
  | (None, h::t) -> List.rev mlst
  | (Some x, h::t) -> get_lst mlst [] x
  | (_, []) -> []


let get_messages uname last (cr : chatroom) =
  cr.name |>?? fun _ ->
  if List.mem uname cr.participants then
    let msgs = Hashtbl.find rooms cr.name |> snd in
    let msgs' = msgs |> shorten_messages last in
    (Messages msgs', Success)
  else (Nothing, Fail "you don't have access to that room")

let get_room i crname =
  crname |>?? fun _ ->
  let cr = Hashtbl.find rooms crname |> fst in
  if List.mem i cr.participants then
    (Chatroom cr, Success)
  else (Nothing, Fail "You don't have access to that room")

let get_users () =
  let fold i _ lst = i :: lst in
  let lst = Hashtbl.fold fold clients [] in
  (Users lst, Success)

(*[handle_get_game id grname] returns a Gamestate response with the 
 *gameroom and board of the desired game as long as user [id] is a 
 *valid player in the game and grname is a valid game*)
let handle_get_game id grname =
  if Hashtbl.mem games grname then 
    let g = Hashtbl.find games grname in 
    if List.mem id g.gr.players then 
      (Gamestate (g.gr,g.board), Success)
    else (Nothing, Fail "invalid id for this game")
  else (Nothing, Fail "invalid game")

(*[replace_nth lst n x] returns the square list lst with the nth
 *element replaced with x (starting from n=0). Cannot replace a
 *square that has already been played (i.e. occupied by X or O)
 *requires: 0 <= n < List.length lst*)
let rec replace_nth lst n x = match lst with 
  | h::t -> if n=0 then 
              if h=N then x::t else failwith "invalid move"
            else h::(replace_nth t (n-1) x)
  | [] -> failwith "failure in replace_nth"

(*[update_wl wid lid] updates the win loss ratios of the winner [wid]
 *and loser [lid]*)
let update_wl wid lid = 
  let wui = Hashtbl.find clients wid in 
  let lui = Hashtbl.find clients lid in 
  let w,l' = wui.wl in 
  let w',l = lui.wl in 
  wui.wl <- (w+1,l') ;
  lui.wl <- (w',l+1)

(*[get_wl id] returns a Wl response with the user [id]'s win-loss info
 *Sends a failure message if the user is invalid*)
let get_wl id =
  if Hashtbl.mem clients id
    then let (w,l) = (Hashtbl.find clients id).wl in  
    (Wl (w,l), Success)
  else (Nothing, Fail "Invalid user")  

(*[change_game_st id gr sq_num] updates the game board of [gr] at the square
 *specified by [sq_num] with either X or O depending on [id]. Also makes 
 *sure that it is [id]'s turn and that the move is a valid one (i.e. it
 *is a square on the board and the square is not already filled*)
let change_game_st id gr sq_num = 
  let st = (Hashtbl.find games gr.name).board in 
  let p1::p2::[] = gr.players in 
  (*assigns X or O to each player*)
  let xo = if id=p1 then X else if id=p2 then O else N in 
  if xo=N then (Nothing, Fail "Invalid player in game\n\n") 
  else try
    let lt = (Hashtbl.find games gr.name).last_turn in 
    (*checks it is [id]'s turn or if no moves have been made either player
     *can make the first move*)
    if Some id <> lt || lt = None then  
      let st' = replace_nth st sq_num xo in 
      (Hashtbl.replace games gr.name {gr=gr;board=st';last_turn=Some id} ;
      (if check_victory st' X then update_wl p1 p2   
      else if check_victory st' O then update_wl p2 p1 
      else ()) ;  
      (Nothing, Success)) 
    else (Nothing, Fail "It's not your turn\n\n")
  with Failure "invalid move" -> (Nothing, Fail "Invalid move\n\n")

(*[reset_game id gr] resets the game [gr] with an empty board and no last_turn*)
let reset_game id gr = 
  let empty = [N;N;N;N;N;N;N;N;N] in 
  if List.mem id gr.players then 
    (Hashtbl.replace games gr.name {gr=gr;board=empty;last_turn=None} ;
    (Nothing, Success))
  else (Nothing, Fail "Invalid player in game\n\n")

let add_to_room u t crname =
  crname |>?? fun _ ->
  t |>? fun _ ->
  let (cr, messages) = Hashtbl.find rooms crname in
  if cr.participants |> List.mem t |> not then
    let cr' = {cr with participants = t :: cr.participants} in
    let conflicts = check_room cr' in
    if 0 = List.length conflicts then
      (Hashtbl.replace rooms crname (cr', messages);
      (Nothing, Success))
    else 
      let err_msg = conflicts |> list_to_string in
      (Nothing, Fail (err_msg))
  else (Nothing, Fail (t ^ " is already in the room"))

let leave_room u crname = 
  u |>? fun _ ->
  crname |>?? fun _ -> 
  let (cr, msg_hist) = Hashtbl.find rooms crname in
  let p' = cr.participants |> List.filter ((<>) u) in
  if List.length p' <> List.length cr.participants then
    let cr' = {cr with participants = p'} in
    Hashtbl.replace rooms crname (cr', msg_hist);
    (Nothing, Success)
  else (Nothing, Fail(u ^ " is not in room " ^ crname))
    

(***************************************************)
(****** end of handlers and helpers *****************)
(***************************************************)
let handle_request req oc =
  match req |> req_from_string with
  | Login identifier -> handle_login identifier oc
  | Register (identifier, pswd) -> handle_reg identifier pswd oc 
  | Auth (identifier, pswd) -> handle_auth identifier pswd oc
  | Block (user, target) -> handle_block user target
  | Unblock (user, target) -> handle_unblock user target
  | Message msg -> post_message msg
  | Listrooms identifier -> get_rooms identifier
  | Listmessages (identifier, last, cr) -> get_messages identifier last cr
  | Newroom cr -> post_room cr
  | Getroom (identifier, crname) -> get_room identifier crname
  | Listusers -> get_users ()
  | Newgame gr -> post_game gr
  | Listgames id -> get_games id 
  | Getwl id -> get_wl id 
  | Getgame (id, grname) -> handle_get_game id grname
  | Changegamest (id, gr, st) -> change_game_st id gr st  
  | Resetgame (id,gr) -> reset_game id gr 
  | AddToRoom (user, target, crname) -> add_to_room user target crname
  | LeaveRoom (user, crname) -> leave_room user crname

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

let get_my_addr () =
  let addr = 
    match Server_args.obj with
    | Web -> 
        (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0)
    | Local -> Unix.inet_addr_loopback in
  let s = Unix.string_of_inet_addr addr in
  Lwt.async ( fun () ->
    ("attempting to run on port 3110 of " ^ s)
    |> Lwt_io.write_line Lwt_io.stdout
  );
  addr

(* assume this just creates the socket ...*)
let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock @@ ADDR_INET(get_my_addr (), 3110);
  listen sock backlog;
  sock

let server =
  let soc = create_socket () in
  create_server soc

let () =
  Lwt_main.run (server ())

