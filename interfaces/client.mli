open Types
open Unix

(* [create_id identifier] creates a request out
 * of the user's identifier *)
val create_id : string -> request

(* [create_message msg] creates a Send request
 * from [msg] *)
val create_message : string -> request

(* [process_response response] handles a
 * response from the server, printing information
 * as necessary *)
val process_response : response -> unit

(* [send_request rqst ()] sends a request to
 * the server.*)
val send_request : request -> unit

(* [parse_command cmnd] takes a string and
 * identifies which command it corresponds to.
 * It then applies the appropriate handler function
 * to the content of the command to get a request. *)
val parse_command : string -> request

(* [repl] is the loop which accepts and processes
 * user commands. It should start by prompting
 * for a user id *)
val repl : unit

