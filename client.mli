type broadcast_message = { msg : bytes; }                                                  
type id_message = { id : bytes; }
type message = BroadcastMessage of broadcast_message | IdMessage of id_message
val clients : ('_a, '_b) Hashtbl.t = <abstr> 
val add_client : id_message -> Lwt_io.output_channel -> unit = <fun>
val send : bytes -> Lwt_io.output_channel -> unit = <fun>
val send_all : broadcast_message -> unit = <fun>
val create_id_message : bytes -> message = <fun>
val create_broadcast_message : bytes -> message = <fun>
val parse_message : bytes -> message = <fun>
val handle_message : bytes -> Lwt_io.output_channel -> unit = <fun>
val handle_connection :
  Lwt_io.input_channel -> Lwt_io.output_channel -> unit -> unit t = <fun>
val accept_connection : Lwt_unix.file_descr * 'a -> unit t = <fun>