open Unix
open Lwt

let main_client client_fun =
	if Array.length Sys.argv < 3
	then Printf.printf "usage : client server port\n"
	else let server = Sys.argv.(1) in 
		let server_addr = 
			try Unix.inet_addr_of_string server
			with Failure("inet_addr_of_string") ->
				try (Unix.gethostbyname server).Unix.h_addr_list.(0)
				with Not_found ->
					Printf.eprintf "%s : Unknown server\n" server ;
					exit 2
		in try
			let port = int_of_string (Sys.argv.(2)) in
			let sockaddr = Unix.ADDR_INET(server_addr,port) in
			let ic,oc = Lwt_io.open_connection sockaddr |> Lwt_main.run 
			in client_fun ic oc ;
			(*shutdown_connection ic *)
			Lwt_io.close ic |> Lwt_main.run 
		with
		| Failure("int_of_string") -> Printf.eprintf "bad port number" ; exit 2

let client_fun ic oc =
	try 
		while true do 
			(*flush (Unix.out_channel_of_descr stdout) ;
			output_string oc ((input_line (Unix.in_channel_of_descr stdin))^"\n") ;
			flush oc ;*)
			let s = (read_line ())^"\n" in
			(*output_string oc s ;*)
			Lwt_io.write_line oc s |> Lwt_main.run ; 
			Lwt_io.flush oc |> Lwt_main.run ; 
			let r = (*read_line ()*) (*input_line ic*) Lwt_io.read_line ic |> Lwt_main.run 
			(*in Printf.printf "Response : %s\n\n" r ;*)
			(*Lwt_io.printl r*)
			in Printf.printf "Response : %s\n\n" r ;  
			(*if r = "END" then (shutdown_connection ic ; raise Exit) ;*)
			if r = "END" then (Lwt_io.close ic |> Lwt_main.run ; raise Exit) ;
		done 
	with 
		| Exit -> exit 0 
		(*| exn -> shutdown_connection ic ; raise exn*)
		| exn -> Lwt_io.close ic |> Lwt_main.run ; raise exn 

let go_client () = main_client client_fun 

let _ = go_client () 