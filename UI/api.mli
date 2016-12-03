open Type_info
open Lwt
(* functions in this file will be called
 * in order to make requests and get the responses in
 * an abstract, UI independent form. The front end is
 * responsible for displaying the responses.
 *)

(* A type representing all the context of the current
 * session *)


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

module MakeRequester : RequesterMaker
