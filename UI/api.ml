open Type_info
open Lwt
open Dummy_client

(* This module is nothing more
 * than a utility module for communicating 
 * with the server. All it does is takes the
 * necessary information for a request, sends
 * out the request, and returns the deferred
 * response *)

exception ServerError
exception ClientError

module type Client = sig

  (*val init : unit -> unit*)

  val init : string -> int -> (request -> response Lwt.t) Lwt.t

  (*val send_req : request -> response Lwt.t*)

end

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

  val add_user_to_room : id -> id -> string -> success Lwt.t
end


module type RequesterMaker =
 functor (Cl : Client ) -> Requester

module MakeRequester (Cl : Client) = struct

  let send_req =  
    let h, p =
      if Array.length Sys.argv > 2 then (Sys.argv.(1), Sys.argv.(2))
      else ("localhost", "3110") in
    Lwt.async (fun () -> 
      let s = "connected to " ^ h ^ " at port " ^ p in
      Lwt_io.write_line Lwt_io.stdout s); 
    let sender = Cl.init h (int_of_string p) in
    (fun req -> sender >>= fun s -> s req)

  (* A wrapper for response handling that raises a useless
   * error if the request was unsuccessful *)
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
      | Chatroom cr -> cr | _ -> raise ClientError in
    send_req req >|= fun (r, succ) ->
    (f r, succ)
   
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

  let send_message identifier cr content = (*string -> success*)
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

  let add_user_to_room user target crname =
    let req = AddToRoom (user, target, crname) in
    send_req req >|= snd

end

