open Type_info
open Lwt
(*open Dummy_client*)

(* This module is nothing more
 * than a utility module for communicating 
 * with the server. All it does is takes the
 * necessary information for a request, sends
 * out the request, and returns the useful 
 * contents of the deferred response  *)

exception ServerError
exception ClientError

(*module type Client = sig


  (* [init host port] will ultimately yield a
   * function for making requests. Because of the
   * nature of the Unix library, the first deferred
   * evaluates when the connection is accepted, and the
   * second is evaluated at every subsequent response.
   * Thus, every call to init will open a new connection
   * with the server. What this means is that init should
   * be used with care*)
  val init : string -> int -> (request -> response Lwt.t) Lwt.t

end*)

module type Requester = sig 

  val login : id -> success Lwt.t

  val register: id -> string-> success Lwt.t

  val auth : id -> string-> success Lwt.t

  val see_chatrooms : id -> Type_info.chatroom list Lwt.t

  val see_users : unit -> id list Lwt.t

  val see_messages : id -> Type_info.msg option -> Type_info.chatroom -> msg list Lwt.t

  val block_user : id -> id -> success Lwt.t

  val unblock_user : id -> id -> success Lwt.t

  val send_message : id -> Type_info.chatroom -> string -> (Type_info.msg * success) Lwt.t

  val get_room : id -> string -> (Type_info.chatroom * success) Lwt.t

  val new_room : id list -> string -> success Lwt.t

  val new_game : id list -> string -> success Lwt.t 

  val see_games : id -> Type_info.gameroom list Lwt.t 

  val get_game : id -> string -> ((Type_info.gameroom * Type_info.square list) * success) Lwt.t 

  val add_user_to_room : id -> id -> string -> success Lwt.t

  val leave_room : id -> string -> success Lwt.t

  val fill_board : id -> Type_info.gameroom -> int -> success Lwt.t
  
  val reset_board : id -> Type_info.gameroom -> success Lwt.t 

  val getwl : id -> (int*int) Lwt.t 
end


module type RequesterMaker =
 functor (Cl : Chat_client.Client ) -> Requester

module MakeRequester (Cl : Chat_client.Client) = struct

  (*[send_req req] is what the api uses to send
   * requests. It has to be crafted delicately so
   * that it doesn't continuously perform Cl.init.
   * Instead, observe the closure that is created
   * in order to recycle the value of the one call
   * to Cl.init. If the program arguments are malformatted
   * the program exits here. *)
  let send_req =  
    try
      let open Client_args in
      let {host = h; port = p} = Client_args.obj () in
      Lwt.async (fun () -> 
        let s = "connecting to " ^ h ^ " at port " ^ p in
        Lwt_io.write_line Lwt_io.stdout s); 
      let sender = Cl.init h (int_of_string p) in
      (fun req -> sender >>= fun s -> s req)
    with
    | Failure s -> print_endline s; exit 1

  (* A wrapper for response handling that raises a
   * vague error if the request was unsuccessful. In
   * general, this should only be used when there is
   * no known reason that the request should fail.
   *)
  let handle_response f (cont, succ) =
    if succ=Success then f cont
    else raise ServerError

  let login identifier =
    let req = Login identifier in
    send_req req >|= snd

  let register id pswd = 
    let req = Register (id,pswd) in 
    send_req req >|= snd

  let auth identifier pswd =
    let req = Auth (identifier, pswd) in
    send_req req >|= snd

  let see_chatrooms identifier =
    let req = Listrooms identifier in
    let f = function
      | Chatrooms clst -> clst | _ -> raise ClientError in
    send_req req >|= (handle_response f)

  let see_users () =
    let f = function
      | Users lst -> lst | _ -> raise ClientError in
    send_req Listusers >|= (handle_response f)
 
  let get_room identifier crname =
    let req = Getroom (identifier, crname) in
    let f = function
    | (Chatroom cr, Success) -> (cr, Success)
    | (_, Success) -> raise ClientError
    | (_,Fail s) -> ({participants = []; name = ""}, Fail s) in
    send_req req >|= f
   
  let see_messages identifier last cr = (* () -> msg list*)
    let f = function
      | Messages lst -> lst | _ -> raise ClientError in
    send_req (Listmessages (identifier, last, cr)) >|= 
    (handle_response f)

  let block_user identifier target = (*id -> success*)
    let req = Block (identifier, target) in
    send_req req >|= snd

  let unblock_user identifier target =
    let req = Unblock (identifier, target) in
    send_req req >|= snd

  let send_message identifier cr content =
    let cont = {
      user = identifier;
      room = cr;
      message = content;
      timestamp = Unix.time ();
    } in
    let req = Message cont in
    send_req req >|= fun resp ->
    (cont, snd resp)

  let new_room members crname =
    let req = Newroom {
      name = crname;
      participants = members;
    } in
    send_req req >|= snd
  
  (*[new_game members gname] sends a Newgame request for a new game
   *with name [gname] and members [members]. Gets Nothing as response*)
  let new_game members gname = 
    let req = Newgame {
      name = gname ;
      players = members }
    in send_req req >|= snd 
  
  (*[see_games id] sends a Listgames request with [id] attached and gets back
   *a Gamerooms response with the list of games the user represented by [id]
   *is in*)
  let see_games id = 
    let req = Listgames id in
    let f = function
    | Gamerooms glst -> glst 
    | _ -> raise ClientError 
    in send_req req >|= (handle_response f)

  (*[get_game id grname] sends a Getgame request and gets back either a
   *Gamestate response if the user is a player in the game or Nothing if not.*)
  let get_game id grname =
    let req = Getgame (id, grname) in
    let f = function
      | (Gamestate (gr,st), Success) -> ((gr,st), Success) 
      | (_, Success) -> raise ClientError
      | (_, Fail s) -> (({name="";players=[]},[]),Fail s)
    in 
    send_req req >|= f

  (*[fill_board id gr sq_num] sends a Changegamest request to change the square
   *represented by [sq_num] in game [gr] to X or O depending on [id]. Gets
   *Nothing as a response.*)
  let fill_board id gr sq_num = 
    let req = Changegamest (id, gr, sq_num) in 
    send_req req >|= snd 

  (*[reset_board id gr] sends a Resetgame request to reset the game [gr] 
   *Gets Nothing as a response.*)
  let reset_board id gr = 
    let req = Resetgame (id,gr) in 
    send_req req >|= snd 

  (*[getwl id] sends a Getwl request for the user represented by [id]. Gets
   *Wl as a response containing the win-loss information.*)
  let getwl id = 
    let req = Getwl id in 
    let f = function 
    | Wl (w,l) -> (w,l) 
    | _ -> raise ClientError 
    in send_req req >|= (handle_response f)   

  let add_user_to_room user target crname =
    let req = AddToRoom (user, target, crname) in
    send_req req >|= snd

  let leave_room user crname =
    let req = LeaveRoom (user, crname) in
    send_req req >|= snd

end

