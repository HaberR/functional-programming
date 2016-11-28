open Unix
open Lwt
(* type friends = string array *)
type  msg = {sender: string ; message: string}
type messages = {sent: msg list ; received: msg list }
type user = {
	oc: Lwt_io.output_channel;
	mutable name:string; 
	mutable pswd:string ; 
	mutable friends:string array ;
	mutable messages: messages
	}
(* type users = string array *)
type  room = {mutable users: user array }

(* type oc = Lwt_io.output_channel *)

let create_room ()= 
	let db={users=[||]} in
	db

(* All clients connected to the server *)
let get_users (db:room) =
	let user_list = db.users in 
	let lst = ref [] in 
	for i = 0 to Array.length user_list - 1 do
		lst := user_list.(i).name::!lst
	done; !lst   

	(* let rec find users = 
		match users with
		 | [] -> []
		 | h::t -> h.name::(find t)
	in find user_list *)

(* default password is "1234" then the client can change it later *)
let create_user name oc =
	let new_user = {
		oc = oc;
		name = name; pswd = "1234" ; 
		friends = [||] ; 
		messages = {sent = []; received = []}} in 
	new_user

let add_user (db:room) name oc = 
	let in_room = get_users db in 
	if(List.mem name in_room) then ()
	else let new_user = create_user name oc in 
	db.users<- new_user :: db.users

let get_oc (db:room) =
	let user_list = db.users in 
	let lst = ref [] in 
	for i = 0 to Array.length user_list - 1 do
		lst := user_list.(i).oc::!lst
	done; !lst   

let find_user (db:room) oc = 
	let arr = db.users in 
	let rec find arr oc n =
			if arr.(n).oc = oc then arr.(n)
			else find arr oc (n+1)
	in find arr oc 0
	(* let lst = get_oc db in
	if(List.mem oc lst) then 
		let rec find lst oc n =
			if lst.(n) = oc then lst.(n)
			else find lst oc (n+1)
		in find lst oc 0
	else ()
 *)

let add_friend (db:room) name oc =
	let usr = find_user db oc in 
	if(Array.mem name usr.friends) then ()
	else usr.friends <- Array.append usr.friends (Array.make 1 name)

let add_message db oc msg = 
	let usr = find_user db oc in 
	

(* let create_table dbh =

	PGSQL(dbh) "execute" "create temporary table users
	(
	name varchar not null primary key,
	pasword text not null,
	friends varchar[]
	)"
let insert_user dbh name pasword =
	PGSQL(dbh) "INSERT INTO users (name, pasword)
	VALUES ($name, $pasword)"

let get_users dbh =
	PGSQL(dbh) "SELECT name FROM users"

let get_friends dbh name = 
	PGSQL(dbh) "SELECT friends FROM users  WHERE name = $name"

let print_user (id, name, age) =
	Printf.printf "Id: %ld Name: %s Age: %ld \n" id name age
let _ =
	let dbh = PGOCaml.connect () in
	let () = create_table dbh in
	let () =
	insert_user dbh "John" 30l;
	insert_user dbh "Mary" 40l;
	insert_user dbh "Mark" 42l in
	List.iter print_user (get_users dbh)

let  *)