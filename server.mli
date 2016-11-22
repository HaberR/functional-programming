open Types
open Unix
open Datastruct

(* [send message oc] sends the message to the user
 * specified by the output channel [oc] *)
val send : msg -> out_channel -> unit

(* [handle_input request oc] processes the request and
 * writes to the oc as necesssary *)
val handle_request : request -> out_channel -> unit

(* [service ic oc, ()] is the service that we have defined. 
 * It reads requests and sends them to handle_request to be
 * processed.*)
val service : in_channel -> out_channel -> unit -> unit

(* [make_server ic oc ()] launches an arbitrary service on port 3110*)
val make_server : in_channel -> out_channel -> unit

