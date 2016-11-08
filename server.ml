open Lwt
open Client

let backlog = 10

let create_server sock =
  let rec serve () =
    Lwt_unix.accept sock >>= accept_connection >>= serve
  in serve

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock @@ ADDR_INET(Unix.inet_addr_loopback, 3110);
  listen sock backlog;
  sock

let server =
  let soc = create_socket () in
  create_server soc


let () =
  Lwt_main.run (server ())

