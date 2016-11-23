open Printf
module T = ANSITerminal
open T

type mode = Normal | Insert

let setting = ref Normal

let parse_insert keystroke =
  failwith "unimplimented"

let parse_normal keystroke = 
  match keystroke with
  | 'h' | 'j'

let handle_stroke key = 
  match !setting with
  | Normal -> parse_normal key
  | Insert -> parse_insert key


let setup_screen () =
  T.erase T.Screen;
  T.set_cursor 3 5;
  T.print_string [Foreground T.Red] "Messenger";
  T.set_cursor 10 2;


let () =
    
  read_line ()

  (*
  let x, y = T.size() in
  printf "The size of the terminal is (%i,%i).%!" x y;
  let x, y = T.pos_cursor() in
  printf "\nThe cursor position was (%i,%i).\n%!" x y;
  T.set_cursor 3 5;
  printf "*<--- set_cursor 3 5";
  T.set_cursor 1 8;
  printf "Press ENTER to temporarily move the cursor to (3,6)%!";
  ignore(read_line());
  T.save_cursor();
  T.set_cursor 3 6;
  printf "*<--- set_cursor 3 6";
  T.restore_cursor();

  for i = 5 downto 1 do
    printf "%i%!" i;
    Unix.sleep 1;
    T.move_bol();
  d*)
