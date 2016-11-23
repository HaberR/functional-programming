open Api
open Types
open Lwt
open Str
(* The backbone of the client side. This should
 * have (and make available) all the information
 * that a UI would need to display the current state *)
module MakeInterface (Quester : Api.Requester) = struct

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

  let log_on identifier = 
    Quester.login identifier >|= fun succ ->
      (if succ then
        current_state := return {
        mode = General;
        info = { username = identifier };
        status = []
        }); succ
  (************ end init stuff ***********************)

  (************ process command and helpers **********)

  let display_users () = 


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

  let rec repl () = 
    let input = read_line () in
    process_input input; repl ()

  let rec run () =
    let input = read_line () in
    log_on input >>= fun succ ->
      if succ then repl ()
      else run ()

end

(* module M = MakeInterface(Blah)
M.repl () *)
