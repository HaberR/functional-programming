open Json

(* type message = {sender : string ; message : string} *)

(*
let rec repl () =

let connect id = 
  *)


let initiate machine port = 
  print_string "enter your id: ";
  print_string machine;
  print_string port

let _ = initiate Sys.argv.(1) Sys.argv.(2)
