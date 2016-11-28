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
    last : Type_info.msg option;
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

  let set_chat cr_and_last = 
    current_state := {
      mode = Inchat cr_and_last;
      info = !current_state.info;
      status = !current_state.status;
    }
  (************ end init stuff ***********************)

  let lread () =
    Lwt_io.read_line Lwt_io.stdin

  let lprint s =
    Lwt_io.write Lwt_io.stdout s 

  let command_prompt () =
    lprint "\n>> "

  let message_prompt () =
    lprint "\n"

  (* Displays the prompt for login *)
  let login_prompt () = 
    lprint "id: "

  let print_in_place lst txt =
    let x, y = T.pos_cursor () in
    T.print_string lst txt;
    T.set_cursor x y

  (* Displays an arbitrary list with an arbitrary
   * print function. No requirement on type of
   * list element *)
  let display_list prnt lst =
    let rec print_seq = function
      | h :: t -> lprint "\n" >>= fun _ ->
          prnt h >>= fun _ -> print_seq t
      | [] -> () |> return in
    print_seq lst

  (* Displays a list with a title. Requires
   * that list elements be strings *)
  let display_title_list title lst =
    lprint title >>= fun _ ->
    let lst' = lst |> List.map ((^) "\t") in
    display_list lprint lst'
  
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
    let disp = display_list lprint in
    ulst |> disp 

  (* I actually need to have that type annotation there
   * to get this thing to compile... Not a great sign *)
  let handle_ls_rooms () =
    Quester.see_chatrooms !current_state.info.username >>= fun clst ->
    let disp_rm ({name = nm; participants = mems} : Type_info.chatroom) = 
      display_title_list nm mems in
    let disp_all lst =
      display_list disp_rm lst in
    clst |> disp_all 

  let handle_enter_room s =
    let nm = Str.matched_group 1 s in
    let uid = !current_state.info.username in
    Quester.get_room uid nm >>= fun ((crm : Type_info.chatroom), succ) ->
    match succ with
    | Success -> current_state := {
      mode = Inchat {
        last = None;
        cr = crm
      };
      info = {username = uid};
      status = !current_state.status
    }; lprint ("entered " ^ crm.name)
    | Fail s -> lprint s

  let handle_exit_room () = 
    let uid = !current_state.info.username in
    (current_state := {
      mode = General;
      info = {username = uid};
      status = !current_state.status
    }) |> return

  let print_message usr msg = 
    (let uid = !current_state.info.username in
    if usr <> uid then lprint (usr ^ ": ")
    else return ()) >>= fun _ ->
    lprint msg


  (*returns the reversed list of all messages
   * more recent than m in mlst. Requires
   * that mlst be sorted such that most recent
   * messages are first *)
  let shorten_messages m mlst =
    let rec get_lst lst acc m =
      match lst with
      | h :: t -> 
          if h = m then acc 
          else get_lst t (h :: acc) m
      | [] -> [] in
    match m,mlst with
    | (None, h::t) -> (Some h, List.rev mlst)
    | (Some x, h::t) -> (Some h, get_lst mlst [] x)
    | a -> a

  let refresh_messages {cr = c; last = m} =
    let uid = !current_state.info.username in
    Quester.see_messages uid c >>= fun mlist ->
    let (m', mlist') = shorten_messages m mlist in
    let p (cont: Type_info.msg) = 
      print_message cont.user cont.message in
    set_chat {last = m'; cr = c};
    mlist' |> display_list p


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

  let handle_send_message {cr = c; last = _} s = 
    let uid = !current_state.info.username in
    Quester.send_message uid c s >>= function
    | Success -> set_chat {last = Some s; c}; return ()
    | Fail b -> lprint b


  (************ End Formatting and printing **********)

  (************ process command and helpers **********)



  let process_command () =
    command_prompt () >>= lread >>= fun s ->
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

  let process_msg cht =
    message_prompt () >>= lread >>= function
    | "\\exit" -> handle_exit_room ()
    | "\\r" -> refresh_messages cht
    | s -> handle_send_message cht s


  let process_input () = 
    match !current_state.mode with
    | Inchat cht -> process_msg cht
    | General -> process_command ()
    (*let cmd_re = Str.regexp "\\(^\\\\[a-zA-Z]+\\).*" in
    if Str.string_match cmd_re input 0 then
      let delim = Str.group_end 1 in
      let keyword = Str.string_before input delim in
      let additional = Str.string_after input (delim + 1) in
      process_command (keyword, additional)
    else process_msg input*)

  let rec repl () = 
    (*command_prompt () >>= fun _ ->
    Lwt_io.read_line Lwt_io.stdin >>=*)
    process_input () >>= repl

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
