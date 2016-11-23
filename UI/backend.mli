open Api
open Types
open Lwt

module type Model = sig
  type t

  type chat = {
    cr : chatroom;
    hist : msg list;
  }

  type information = {
    username : id
  }

  type md = Inchat of chat | General

  type cmnd_status = (string * success) list

  type state = {
    mode : md;
    info : information;
    status : cmnd_status 
  }

  val init : string -> success Lwt.t

  val get_state : unit -> state Lwt.t

  val process_input : string -> unit

end

module type ModelMaker =
  functor (Quester : Api.Requester) -> Model

module MakeModel : ModelMaker
