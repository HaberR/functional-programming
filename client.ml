open Lwt

(* type message = {sender : string ; message : string} *)

type broadcast_message = {
  msg: string
}

type id_message = {
  id: string
}

type message =
  | BroadcastMessage of broadcast_message
  | IdMessage of id_message


let create_id_message str =
  IdMessage {id=String.sub str 3 (String.length str - 3)}

let create_broadcast_message str =
  BroadcastMessage {msg=str}


