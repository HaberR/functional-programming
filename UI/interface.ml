open Api
open Types
open Lwt
open Str

module T = ANSITerminal
(* The backbone of the client side. This should
 * have (and make available) all the information
 * that a UI would need to display the current state *)
module MakeInterface (Quester : Api.Requester) = struct

  (********** Reiterating a bunch of type info ******)
  type chat = {
    cr : Type_info.chatroom;
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

  type t = state ref

  (*********** End type info ************************)

  (*********** Basic init stuff *********************)
  (* placeholder current state until it
   * get set in init *)
  let current_state = ref ( {
    mode = General;
    info = { username = "nothing"};
    status = []
  })

  let log_on identifier = 
    Quester.login identifier >|= fun succ ->
      (if succ=Success then
        current_state := {
        mode = General;
        info = { username = identifier };
        status = []
        }); succ
  (************ end init stuff ***********************)

  let lread () =
    Lwt_io.read_line Lwt_io.stdin

  let lprint s =
    Lwt_io.write Lwt_io.stdout s 

  let command_prompt () =
    lprint "\n>> "

  (* Displays the prompt for login *)
  let login_prompt () = 
    lprint "id: "

  let print_in_place lst txt =
    let x, y = T.pos_cursor () in
    T.print_string lst txt;
    T.set_cursor x y

  let display_list title prnt lst =
    lprint title >>= fun _ ->
    let rec print_seq = function
      | h :: t -> lprint "\n\t" >>= fun _ ->
          prnt h >>= fun _ ->
          print_seq t
      | [] -> () |> return in
    print_seq lst

  let handle_new_room () =
    lprint "room name: " >>= command_prompt >>=
    lread >>= fun rmname ->
    lprint "participant list: " >>= command_prompt >>=
    lread >>= fun plst ->
    let lst = plst |> Str.split (Str.regexp " ") in
    Quester.new_room lst rmname >>= function
    | Success -> return ()
    | Fail s -> lprint s
    

  let handle_ls_users () =
    Quester.see_users () >>= fun ulst -> 
    let disp = 
      display_list "Users registered:" lprint in
    ulst |> disp 

  (* I actually need to have that type annotation there
   * to get this thing to compile... Not a great sign *)
  let handle_ls_rooms () =
    Quester.see_chatrooms !current_state.info.username >>= fun clst ->
    let disp_rm ({name = nm; participants = mems} : Type_info.chatroom) = 
      display_list nm lprint mems in
    let disp_all lst =
      display_list "Chatrooms:" disp_rm lst in
    clst |> disp_all 

  let handle_enter_room s =
    let nm = Str.matched_group 1 s in
    let uid = !current_state.info.username in
    Quester.get_room uid nm >>= fun ((crm : Type_info.chatroom), succ) ->
    match succ with
    | Success -> current_state := {
      mode = Inchat {
        hist = [];
        cr = crm
      };
      info = {username = uid};
      status = !current_state.status
    }; lprint ("entered " ^ crm.name)
    | Fail s -> lprint s

  let refresh_messages () =
    failwith "unimplimented"

  (*let handle_open crname = 
    let uid = !current_state.info.username in
    Quester.get_room uid crname >>= fun croom ->
    current_state := {
      mode = Inchat {
        hist = []
        cr = croom
      };
      info = {username = uid};
      status = []
    }  TODO make refresh messages start *)


  (************ End Formatting and printing **********)

  (************ process command and helpers **********)



  let process_command s =
    let sm re = 
      let re' = Str.regexp re in
      Str.string_match re' s 0 in
    if      "^ls users" |> sm then handle_ls_users ()
    else if "^ls rooms" |> sm then handle_ls_rooms ()
    else if "^new room" |> sm then handle_new_room ()
    else if "^enter \\(.*\\)" |> sm then handle_enter_room s
    (*else if "^open \\(.*\\)" |> sm then 
    | "open" |> sm -> handle_open ()*)
    else 
        (print_string s;
        failwith "unimplemented")

  let process_msg input =
    failwith "unimplemented"

  let process_input input = 
    match !current_state.mode with
    | Inchat _ -> process_msg input
    | General -> process_command input
    (*let cmd_re = Str.regexp "\\(^\\\\[a-zA-Z]+\\).*" in
    if Str.string_match cmd_re input 0 then
      let delim = Str.group_end 1 in
      let keyword = Str.string_before input delim in
      let additional = Str.string_after input (delim + 1) in
      process_command (keyword, additional)
    else process_msg input*)

  let rec repl () = 
    command_prompt () >>= fun _ ->
    Lwt_io.read_line Lwt_io.stdin >>=
    process_input >>= repl

  let rec run () =
    login_prompt() >>=
    command_prompt >>= fun _ ->
    Lwt_io.read_line Lwt_io.stdin >>=
    log_on >>= fun succ ->
      if succ=Success then repl ()
      else run ()

  let _ = run () |> Lwt_main.run

end

module Dummyquester = MakeRequester(Chat_client_test)
module DummyInterface = MakeInterface(Dummyquester)
(*let _ = DummyInterface.run ()*)
