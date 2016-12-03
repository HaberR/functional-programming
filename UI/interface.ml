open Api
open Type_info 
open Lwt
open Str

(* The backbone of the client side. This should
 * have (and make available) all the information
 * that a UI would need to display the current state *)
module MakeInterface (Quester : Api.Requester) = struct

  (********** Reiterating a bunch of type info ******)
  type chat = {
    cr : Type_info.chatroom;
    last : Type_info.msg option;
  }

  type game = {
    gr : Type_info.gameroom ;
    last_state : Type_info.square list 
  }  

  type information = {
    username : id
  }

  type md = Inchat of chat | Ingame of game | General

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

  let set_game gr_and_last = 
    current_state := {
      mode = Ingame gr_and_last ;
      info = !current_state.info ;
      status = !current_state.status
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

let hide_password () =
  let with_echo = Unix.tcgetattr Unix.stdin in
  let no_echo = { with_echo with Unix.c_echo = false } in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW no_echo;
  try
    let pswd = read_line ()  in
    print_newline ();
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH with_echo;
    pswd
  with e ->
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH with_echo;
    raise e

  let rec handle_register () = 
    lprint "Choose your id: " >>= command_prompt>>= 
    lread >>= fun id -> Quester.login id >>= function
    | Fail b -> print_string "Create Password: " ; let pswd1 = hide_password ()in 
          if (String.length pswd1 >=4) then 
            (print_string "Confirm Password: " ; let pswd2 = hide_password () in 
            if pswd1=pswd2 then (Quester.register id pswd2 >|= fun succ ->
              (if succ=Success then
                current_state := {
                mode = General;
                info = { username = id };
                status = []
                }); succ)
            else (lprint "Passwords not matching!\n") >>= fun _ ->handle_register () )
          else (lprint "Password should be at least 4 characters\n") >>= fun _ ->handle_register () 
    | Success  ->(lprint "This id is already registered to another client\n") >>= fun _ ->handle_register () 

  let log_on identifier =
      Quester.login identifier >|= fun succ -> succ

  let auth_pswd pswd identifier = 
      Quester.auth pswd identifier >|= fun succ -> succ
      
  let set_chat cr_and_last = 
    current_state := {
      mode = Inchat cr_and_last;
      info = !current_state.info;
      status = !current_state.status;
    }
  (************ end init stuff ***********************)

  (* Displays the prompt for login *)
  let login_prompt () = 
    lprint "Enter your (id) or (N) for new user\nid: "

  (*let print_in_place lst txt =
    let x, y = T.pos_cursor () in
    T.print_string lst txt;
    T.set_cursor x y*)
  
  (*[display_game_st st] prints the board represented by the state st
   *with proper formatting*)
  let display_game_st st = 
    let string_of_square sq = match sq with 
    | N -> "_"
    | X -> "X"
    | O -> "O"
    in let format = ref 0 in 
    let rec p st = match st with 
    | h::t -> if !format = 2 then 
      (format := 0 ; 
      lprint (string_of_square h) >>= fun _ ->
      lprint "\n" >>= fun _ -> p t) 
      else (incr format ; lprint (string_of_square h) >>= fun _ ->
      lprint " " >>= fun _ -> p t) 
    | [] ->  return () 
    in p st 

  (* Displays an arbitrary list with an arbitrary
   * print function. No requirement on type of
   * list element *)
  let display_list prnt lst =
    let rec print_seq = function
      | h :: t -> prnt h >>= fun _ ->
          lprint "\n" >>= fun _ -> print_seq t
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
    let lst' = !current_state.info.username :: lst in
    Quester.new_room lst' rmname >>= function
    | Success -> return ()
    | Fail s -> lprint s
    
  let handle_new_game () = 
    lprint "game name: " >>= command_prompt >>=
    lread >>= fun gname ->
    lprint "players: " >>= command_prompt >>=
    lread >>= fun players ->
    let plist = players |> Str.split (Str.regexp " ") in 
    if List.length plist <> 2 then lprint "invalid number of players"
    else Quester.new_game plist gname >>= function 
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
  
  let handle_ls_games () = 
    Quester.see_games !current_state.info.username >>= fun glst ->
    let disp_gm ({name = n ; players = p} : Type_info.gameroom) =
      display_title_list n p in 
    let disp_all lst = 
      display_list disp_gm lst in 
    glst |> disp_all 
  
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

  (* [check_response_validity mlist] takes in a
   * message list and makes sure that the most recent
   * message is not included in the list. If it is,
   * that means the list was created with an old
   * most recent message and the whole list is invalid*)
  let check_response_validity old_last =
    match !current_state.mode with
    | General | Ingame _ -> false
    | Inchat x -> match x.last, old_last with
      | (None, None) -> true
      | (Some a, Some b) -> a = b
      | _ -> false
  
  let check_game_state old_state = 
    match !current_state.mode with 
    | General | Inchat _ -> false 
    | Ingame x -> x.last_state = old_state 

  let rec last_elem old lst = 
    match lst with
    | h1 :: h2 :: t -> last_elem old (h2 :: t)
    | h :: t -> Some h
    | [] -> old

  let refresh_messages {cr = c; last = m} =
    let uid = !current_state.info.username in
    Quester.see_messages uid m c >>= fun mlist ->
    if check_response_validity m then
      (*let (m', mlist') = shorten_messages m mlist in*)
      let last' = last_elem m mlist in
      let mode' = Inchat {cr = c; last = last'} in
      current_state := {!current_state with mode = mode'};
      let p (cont: Type_info.msg) = 
        print_message cont.user cont.message in
      mlist |> display_list p
    else return ()

  let handle_send_message {cr = c; last = _} s = 
    let uid = !current_state.info.username in
    Quester.send_message uid c s >>= fun (msg, succ) ->
    match succ with
    | Success -> set_chat {last = Some msg; cr = c}; return ()
    | Fail b -> lprint b

  let refresh_game {gr = g ; last_state = st} =
    let uid = !current_state.info.username in 
    Quester.get_game uid g.name >>= fun (st,_) ->
    if check_game_state (snd st) then 
      (*(set_game {gr = g ; last_state = (snd st)} ;
      display_game_st (snd st)) *)
      return () 
    else  
      (set_game {gr = g ; last_state = (snd st)} ;
      display_game_st (snd st))

  let mut = Lwt_mutex.create()

  let fork_refresh () =
    Lwt.async (fun () ->
      let rec loop () =
        match !current_state.mode with
        | Inchat x -> refresh_messages x >>= loop
        | Ingame x -> refresh_game x >>= loop 
        | General -> return () in 
      loop ()
    ); return ()

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
      }; lprint ("entered " ^ crm.name ^ "\n") >>= fork_refresh
    | Fail s -> lprint s

  let handle_enter_game s = 
    let nm = Str.matched_group 1 s in 
    let uid = !current_state.info.username in 
    Quester.get_game uid nm >>= fun (((grm : Type_info.gameroom),_), succ) ->
    match succ with 
    | Success -> current_state := {
      mode = Ingame {
        gr = grm ;
        last_state = [N;N;N;N;N;N;N;N;N]
      } ; 
      info = {username = uid} ;
      status = !current_state.status
      } ; lprint ("entered " ^ grm.name ^ "\n") >>= fork_refresh
    | Fail s -> lprint s 

  let handle_leave_room s =
    let crname = Str.matched_group 1 s in
    let uid = !current_state.info.username in
    Quester.leave_room uid crname >>= function
    | Success -> return ()
    | Fail s -> lprint s

  let handle_block s =
    let nm = Str.matched_group 1 s in
    let uid = !current_state.info.username in
    Quester.block_user uid nm >>= function
    | Success -> return ()
    | Fail s -> lprint s

  let handle_unblock s =
    let nm = Str.matched_group 1 s in
    let uid = !current_state.info.username in
    Quester.unblock_user uid nm >>= function
    | Success -> return ()
    | Fail s -> lprint s

  let handle_add_to_room {cr = c; last = l} inpt =
    let nm = Str.matched_group 1 inpt in
    let uid = !current_state.info.username in
    Quester.add_user_to_room uid nm c.name >>= function
    | Success -> 
        let newrm = {c with participants = inpt :: c.participants} in
        let cht' = {cr = newrm; last = l} in
        current_state := {!current_state with mode = Inchat cht'};
        return ()
    | Fail s -> lprint s >>= fun _ -> lprint "\n"

  (************ End Formatting and printing **********)

  (************ process command and helpers **********)


  let str_mtch s re = 
    let re' = Str.regexp re in
    Str.string_match re' s 0

  let process_command () =
    command_prompt () >>= lread >>= fun s ->
    let sm = str_mtch s in
    if      "^ls users" |> sm then handle_ls_users ()
    else if "^ls rooms" |> sm then handle_ls_rooms ()
    else if "^new room" |> sm then handle_new_room ()
    else if "^new game" |> sm then handle_new_game () 
    else if "^ls games" |> sm then handle_ls_games () 
    else if "^play \\(.*\\)" |> sm then handle_enter_game s 
    else if "^enter \\(.*\\)" |> sm then handle_enter_room s
    else if "^leave \\(.*\\)" |> sm then handle_leave_room s
    else if "^block \\(.*\\)" |> sm then handle_block s
    else if "^unblock \\(.*\\)" |> sm then handle_unblock s
    (*else if "^open \\(.*\\)" |> sm then 
    | "open" |> sm -> handle_open ()*)
    else       
        lprint "unrecognized command"

  let process_msg cht =
    lread () >>= fun inpt ->
    if "\\\\add \\(.*\\)" |> str_mtch inpt then 
      handle_add_to_room cht inpt
    else
      match inpt with
      | "\\exit" -> handle_exit_room ()
      | "\\r" -> refresh_messages cht
      | s -> handle_send_message cht s

  let process_game g = 
    lread () >>= function 
    | "\\board" -> display_game_st g.last_state
    | "\\help" -> lprint "\\board to display the current board\n"
    | _ -> lprint "unrecognized command\n"  

  let process_input () = 
    match !current_state.mode with
    | Inchat cht -> process_msg cht
    | Ingame g -> process_game g 
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
    Lwt_io.read_line Lwt_io.stdin >>= fun id ->
    if "^[Nn]" |> str_mtch id then handle_register ()>>=fun _->run()
    else
      log_on id >>= fun succ ->
        if succ=Success then 
          (print_string "\nPassword: "; let pswd = hide_password () in
          auth_pswd id pswd >>= fun succ2 ->
            if succ2=Success then
              repl ()
            else lprint "Wrong Password \n" >>= fun _ -> run() )
        else lprint "This id is not recognized\n"  >>= fun _ -> run ()

  let _ = run () |> Lwt_main.run

end

module Dummyquester = MakeRequester(Chat_client_test)
module DummyInterface = MakeInterface(Dummyquester)
