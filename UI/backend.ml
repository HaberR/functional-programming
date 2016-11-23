open Api
open Types
open Lwt
open Str

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

(* The backbone of the client side. This should
 * have (and make available) all the information
 * that a UI would need to display the current state *)
module MakeModel (Quester : Api.Requester) = struct

  (********** Reiterating a bunch of type info ******)
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

  type t = state Lwt.t ref

  (*********** End type info ************************)

  (*********** Basic init stuff *********************)
  (* placeholder current state until it
   * get set in init *)
  let current_state = ref ( return {
    mode = General;
    info = { username = "nothing"};
    status = []
  })

  let get_state () = !current_state

  let init identifier = 
    Quester.login identifier >|= fun succ ->
      (if succ then
        current_state := return {
        mode = General;
        info = { username = identifier };
        status = []
        }); succ
  (************ end init stuff ***********************)

  (************ process command and helpers **********)

  let 

  let process_command = function (*(key, remaining) =*)
    | ("\\ls"," users") -> failwith "unimplemented"
    | _ -> failwith "unimplemented"

  let process_msg input =
    failwith "unimplemented"

  let process_input input = 
    let cmd_re = Str.regexp "\\(^\\\\[a-zA-Z]+\\).*" in
    if Str.string_match cmd_re input 0 then
      let delim = Str.group_end 0 in
      let keyword = Str.string_before input delim in
      let additional = Str.string_after input delim in
      process_command (keyword, additional)
    else process_msg input

end


