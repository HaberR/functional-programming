(* type xmpp

(* [process_and_respond in_ch out_ch] is the function which will handle
 * all requests and write all responses. It is the crux of the server. 
 * This is the singular function that needs to be implemented--the rest 
 * are helpers. *)
val process_and_respond : Unix.in_channel -> Unix.out_channel -> unit


*****************************************************************
(******     HELPERS   **********************************************)
(*******************************************************************)

val convert : Unix.in_channel -> xmpp

val route : xmpp -> unit

val store : data -> data

val fetch : u_id -> data


(* telnet Benards-MacBook-Pro.local 1400 *)
 *)