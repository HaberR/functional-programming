open Type_info
open Unix
open Lwt

(* [get_requester server port] takes a string [server] and
 * an integer [port] and produces a function request -> response Lwt.t
 * which may be used to send requests to the server at port [port] *)
let init server port =
  (*if Array.length Sys.argv < 3
  then Printf.printf "usage : client server port\n"
  else*)
  let server_addr =
    try Unix.inet_addr_of_string server
    with Failure("inet_addr_of_string") -> 
      try (Unix.gethostbyname server).Unix.h_addr_list.(0)
      with Not_found ->
        Printf.eprintf "%s : unknown server\n" server ;
        exit 2
  in try
    (*let port = int_of_string port in*)
    let sockaddr = Unix.ADDR_INET(server_addr, port) in
    (* oc = output channel of server.
     * ic = input channel of client *)
    (Lwt_io.open_connection sockaddr >|= fun (ic, oc) -> 
      (fun req ->
        let req' = req |> req_to_string in
        Lwt_io.write_line oc (req' ^ "\n") >>= fun () ->
        Lwt_io.flush oc >>= fun () ->
        print_string ("wrote: " ^ req');
        Lwt_io.read_line ic >|= resp_from_string)) |> Lwt_main.run
    (*match Lwt_unix.fork () with 
    | 0 -> client_child_fun oc ; exit 0
    | id -> client_parent_fun ic ; Lwt_io.close ic |> Lwt_main.run ; ignore (Unix.waitpid [] id)*)
  with 
    Failure("int_of_string") -> Printf.eprintf "bad port number" ; exit 2

(*
let send_req (ic, oc) req =
  Lwt_io.write_line oc (req ^ "\n") >>= fun () ->
  Lwt_io.flush oc >>= fun () ->
  print_string ("wrote: " ^ req);
  Lwt_io.read_line ic 
  *)


  (*
let copy_channels ic oc = 
  try while true do
    let s = ((Lwt_io.read_line ic) |> Lwt_main.run)(*^"\n"*)in 
    (Lwt_io.write_line oc (s) |> Lwt_main.run ; Lwt_io.flush oc |> Lwt_main.run)
    done 
  with End_of_file -> ()

let child_fun ic oc =
  copy_channels ic oc ;
  Lwt_io.write_line oc ("FIN\n") |> Lwt_main.run ;
  Lwt_io.flush oc

let parent_fun oc ic = copy_channels ic oc 

let init () =
  main_client (parent_fun Lwt_io.stdout) (child_fun Lwt_io.stdin) ;
  Lwt_io.close Lwt_io.stdin |> Lwt_main.run

*)
