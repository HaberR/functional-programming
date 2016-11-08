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

let clients = Hashtbl.create 100
let add_client msg oc =
  Hashtbl.replace clients msg.id oc

let send msg recipient =
  Lwt_io.write_line recipient msg;
  ()
 

let send_all msg =
  let all_clients = Hashtbl.fold (fun k v acc -> v :: acc) clients [] in
  List.iter (fun s -> send msg.msg s) all_clients


let create_id_message str =
  IdMessage {id=String.sub str 3 (String.length str - 3)}

let create_broadcast_message str =
  BroadcastMessage {msg=str}

let parse_message msg =
  match String.sub msg 0 2 with
  | "id" -> create_id_message msg
  | _ -> create_broadcast_message msg

let handle_message msg oc =
  match parse_message msg with
  | IdMessage m -> add_client m oc
  | BroadcastMessage m -> send_all m
let rec handle_connection ic oc () =
  Lwt_io.read_line_opt ic >>=
    (fun msg ->
      match msg with
      | Some msg -> Lwt.return (handle_message msg oc) >>= handle_connection ic oc
      | None -> Lwt_log.info "Connection closed!" >>= return)
let accept_connection conn =
  (* conn is (fd * sock_addr) pair *)
  let fd, _ = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt.on_failure (handle_connection ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt_log.info "New connection" >>= return


