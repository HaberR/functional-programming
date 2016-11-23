open Types
open Lwt
(* functions in this file will be called
 * in order to make requests and get the responses in
 * an abstract, UI independent form. The front end is
 * responsible for displaying the responses.
 *)

(* A type representing all the context of the current
 * session *)

module type Client = sig

  val init : unit -> unit

  val send_req : request -> resp_wo_head Lwt.t

end

module type Backend = sig 
  type state

  val login : string -> unit Lwt.t

  val see_chatrooms : unit -> chatroom list Lwt.t

  val see_users : unit -> id list Lwt.t

  val see_messsages : unit -> msg list Lwt.t

  val block_user : unit -> success Lwt.t

  val send_message : string -> success Lwt.t

  val enter_room : unit -> unit Lwt.t

  val exit_room : unit -> unit
end

module type BackendMaker =
 functor (Cl : Client ) -> Backend

module MakeBackend : BackendMaker
