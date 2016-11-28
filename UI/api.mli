open Type_info
open Lwt
(* functions in this file will be called
 * in order to make requests and get the responses in
 * an abstract, UI independent form. The front end is
 * responsible for displaying the responses.
 *)

(* A type representing all the context of the current
 * session *)

module type Client = sig

  (*val init : unit -> unit*)

  val init : string -> int -> (request -> response Lwt.t)

  (*val send_req : request -> response Lwt.t*)

end

module type Requester = sig 

  val login : id -> success Lwt.t

  val see_chatrooms : id -> Type_info.chatroom list Lwt.t

  val see_users : unit -> id list Lwt.t

  val see_messages : id -> Type_info.chatroom -> msg list Lwt.t

  val block_user : id -> id -> success Lwt.t

  val send_message : id -> Type_info.chatroom -> string -> (Type_info.msg * success) Lwt.t

  val get_room : id -> string -> (Type_info.chatroom * success) Lwt.t

  val new_room : id list -> string -> success Lwt.t
end

module type RequesterMaker =
 functor (Cl : Client ) -> Requester

module MakeRequester : RequesterMaker
