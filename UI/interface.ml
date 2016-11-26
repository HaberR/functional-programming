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

  let command_prompt () =
    T.move_cursor 0 1;
    T.scroll 1;
    T.move_bol();
    T.print_string [Foreground T.Blue] ">> "

  (* Displays the prompt for login *)
  let login_prompt () = 
    T.move_bol();
    T.print_string [Foreground T.Green] "id: "

  let print_in_place lst txt =
    let x, y = T.pos_cursor () in
    T.print_string lst txt;
    T.set_cursor x y

  (*let display_content display content =
    let (x, y) = T.pos_cursor() in
    clear_display ();
    T.set_cursor 1 disp_top;
    display content;
    T.set_cursor x y*)

  let display_list title prnt lst =
    print_in_place [] title;
    T.move_cursor 5 0;
    let rec print_seq = function
      | h :: t -> 
          (*T.move_cursor 0 1;*)
          T.scroll 1;
          prnt h; 
          print_seq t
      | [] -> () in
    print_seq lst

  let handle_ls_users () =
    Quester.see_users () >>= fun ulst -> 
    let disp = 
      display_list "Users registered:" (print_in_place []) in
    ulst |> disp |> return

  (* I actually need to have that type annotation there
   * to get this thing to compile... Not a great sign *)
  let handle_ls_rooms () =
    Quester.see_chatrooms !current_state.info.username >>= fun clst ->
    let disp_rm ({name = nm; participants = mems} : Type_info.chatroom) = 
      display_list nm (print_in_place []) mems in
    let disp_all lst =
      display_list "Chatrooms:" disp_rm lst in
    clst |> disp_all |> return

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
    command_prompt ();
    let input = read_line () in
    process_input input >>= repl

  let rec run () =
    login_prompt(); command_prompt ();
    let input = read_line () in
    log_on input >>= fun succ ->
      if succ=Success then repl ()
      else run ()

end

module Dummyquester = MakeRequester(Chat_client_test)
module DummyInterface = MakeInterface(Dummyquester)
let _ = DummyInterface.run ()
