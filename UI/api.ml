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

  val init : string -> int -> (request -> response Lwt.t)

  (*val send_req : request -> response Lwt.t*)

end

module type Requester = sig 

  val login : id -> success Lwt.t

  val see_chatrooms : id -> chatroom list Lwt.t

  val see_users : unit -> id list Lwt.t

  val see_messsages : id -> string -> msg list Lwt.t

  val block_user : id -> id -> success Lwt.t

  val send_message : id -> string -> string -> success Lwt.t

  val get_room : id -> string -> chatroom Lwt.t

  val new_room : id list -> string -> success Lwt.t
end


module type RequesterMaker =
 functor (Cl : Client ) -> Requester

module MakeRequester (Cl : Client) = struct

  let send_req = Cl.init "localhost" 3110

  (* A wrapper for response handling that raises a useless
   * error if the request was unsuccessful *)
  let handle_response f (cont, succ) =
    if succ=Success then f cont
    else raise ServerError

  let login identifier =
    let req = Login identifier in
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
    send_req req >|= (handle_response f)
   
  let see_messsages identifier crname = (* () -> msg list*)
    let f = function
      | Messages lst -> lst | _ -> raise ClientError in
    get_room identifier crname >>= fun cr ->
    send_req (Listmessages (identifier, cr)) >|= 
    (handle_response f)

  let block_user identifier target = (*id -> success*)
    let req = Block (identifier, target) in
    send_req req >|= snd

  let send_message identifier content crname = (*string -> success*)
    get_room identifier crname >>= fun cr ->
    let req = Message {
      user = identifier;
      room = cr;
      message = content;
      timestamp = Unix.time ();
    } in
    send_req req >|= snd

  let new_room members crname =
    let req = Newroom {
      name = crname;
      participants = members;
    } in
    send_req req >|= snd

end

