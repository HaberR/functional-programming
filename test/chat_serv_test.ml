open Lwt

(* type message = {sender : string ; message : string} *)

(*message to be sent to other users*)
type broadcast_message = {
  msg: string ; sender: string ; to_friends: bool
}

(*message to server indicating new user*)
type id_message = {
  id: string
}

type message =
  | BroadcastMessage of broadcast_message
  | IdMessage of id_message
  | CommandMessage of string 

type user_info = {
  id: string ; mutable friends: string list
}

let create_id_message str =
  (*use String.sub to ignore the 'id' part of the string*)
  IdMessage {id= try String.sub str 3 (String.length str - 3) with _ -> "bad id"}

let create_broadcast_message str id tf =
  BroadcastMessage {msg=str ; sender=id; to_friends=tf}

let msg_to_string msg = 
  msg.sender ^ ": " ^ msg.msg 

let clients = Hashtbl.create 100
let clientList = ref [] 

let add_client (id_msg : id_message) oc =
  Hashtbl.replace clients id_msg.id oc ;
  clientList := (oc,{id=id_msg.id; friends=[]})::!clientList ;
  Lwt.return () 

let client_lst_to_string lst = 
  "["^(lst |> List.map snd |> List.fold_left (fun acc x -> acc^x.id^"; ") "")^"]"

(*return the list of friends of the user at oc as a string*)
let get_friends oc =
  "["^((List.assoc oc !clientList).friends |> List.fold_left (fun acc x -> acc^x^"; ") "")^"]"

let add_friend msg oc = 
  try 
    let friend = String.sub msg 11 ((String.length msg)-11) in 
    let friends_list = (List.assoc oc !clientList).friends in 
    (List.assoc oc !clientList).friends <- friend::friends_list ;
    CommandMessage "friends list updated"
  with 
    Invalid_argument _ -> CommandMessage "Invalid id to add as friend"

(*let send msg recipient = 
  Lwt_io.write_line recipient (msg_to_string msg)*)
  (*output_string recipient (msg_to_string msg) *)

let send s recipient = 
  Lwt_io.write_line recipient s 

let send_all msg oc =
  (*list of the out channels for all clients*)
  let all_clients = Hashtbl.fold (fun k v acc -> v :: acc) clients [] in
  (*do not send to the user who sent the initial message*)
  List.iter (fun s -> if s==oc then () else let _ = send msg s in ()) all_clients
  |> return 

let send_to_friends msg oc =
  let friends = (List.assoc oc !clientList).friends in 
  let ocs = Hashtbl.fold (fun k v acc -> if List.mem k friends then v::acc else acc) clients [] in 
  List.iter (fun s -> let _ = send msg s in ()) ocs |> return 
  

let parse_message msg oc =
  let id_reg = Str.regexp "id:" in 
  let getUsers_reg = Str.regexp "/getusers" in 
  let getFriends_reg = Str.regexp "/getfriends" in 
  let addFriend_reg = Str.regexp "/addfriend" in 
  let toFriends_reg = Str.regexp "/tofriends" in 
  if Str.string_match id_reg msg 0 then create_id_message msg 
  else if Str.string_match getUsers_reg msg 0 then CommandMessage (client_lst_to_string !clientList)
  else if Str.string_match getFriends_reg msg 0 then CommandMessage (get_friends oc)
  else if Str.string_match addFriend_reg msg 0 then add_friend msg oc 
  else if Str.string_match toFriends_reg msg 0 
    then create_broadcast_message (String.sub msg 10 ((String.length msg)-10)) (List.assoc oc !clientList).id true 
  else create_broadcast_message msg (List.assoc oc !clientList).id false

(*let parse_message msg oc =
  try 
  match String.sub msg 0 3 with
  | "id:" -> create_id_message msg
  | _ -> create_broadcast_message msg (List.assoc oc !clientList)
  with Invalid_argument _ -> create_broadcast_message msg (List.assoc oc !clientList)*)

let handle_message msg oc =
  match parse_message msg oc with
  | IdMessage m -> add_client m oc   
  | BroadcastMessage m when m.to_friends=true -> send_to_friends (msg_to_string m) oc 
  | BroadcastMessage m -> send_all (msg_to_string m) oc 
  | CommandMessage s -> send s oc 

let rec handle_connection ic oc () =
  Lwt_io.read_line_opt ic >>=
    (fun msg ->
      match msg with
      | Some msg -> handle_message msg oc >>= handle_connection ic oc
      | None -> Lwt_log.info "Connection closed!" >>= return)

let accept_connection conn =
  (* conn is (fd * sock_addr) pair *)
  let fd, _ = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt.on_failure (handle_connection ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt_log.info "New connection" >>= return

let backlog = 10

let create_server sock =
  let rec serve () =
    Lwt_unix.accept sock >>= accept_connection >>= serve
  in serve

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

  