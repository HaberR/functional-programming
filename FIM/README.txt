Compile the client application with make.
Compile the server application with make server.

Run the client application with ./interface.byte [-h HOST] [-p PORT]

Run the server application with ./chat_server.byte [-o]
The -o parameter allows you to connect to the server over the internet

You may need to install core and Lwt using 
	opam install core,lwt
