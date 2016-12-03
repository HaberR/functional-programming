open Core.Std

type id = string [@@deriving sexp]

type square = N | X | O [@@deriving sexp]

type chatroom =
  {
    name : string;
    participants : id list;
  } [@@deriving sexp]

type gameroom =
  {
    name : string ; 
    players : id list ;
  } [@@deriving sexp]

type msg = 
  {
    user : string; 
    room : chatroom;
    message : string; 
    timestamp : float
  } [@@deriving sexp]

type success = Success | Fail of string [@@deriving sexp]

(* request is the type of requests that the client
 * may send to the server*)
type request = 
  | Message of msg
  | Register of id * string 
  | Login of id
  | Auth of id * string
  | Block of id * id
  | Unblock of id * id
  | Listrooms of id
  | Listmessages of id * msg option * chatroom
  | Newroom of chatroom 
  | Getroom of id * string
  | Listusers 
  | Newgame of gameroom 
  | Listgames of id 
  | Getgame of id * string 
  | Getwl of id 
  | Changegamest of id * gameroom * int
  | Resetgame of id * gameroom 
  | AddToRoom of id * id * string 
  | LeaveRoom of id * string [@@deriving sexp]

type resp = 
  | Chatroom of chatroom
  | Gameroom of gameroom 
  | Messages of msg list
  | Chatrooms of chatroom list
  | Users of id list 
  | Wl of int * int 
  | Gamerooms of gameroom list 
  | Gamestate of gameroom * square list 
  | Nothing [@@deriving sexp] 

(* the type of the response *)
type response = resp * success [@@deriving sexp]

let req_to_string req = 
  req |> sexp_of_request |> Sexp.to_string

let req_from_string s =
  s |> Sexp.of_string |> request_of_sexp

let resp_to_string resp =
  resp |> sexp_of_response |> Sexp.to_string

let resp_from_string s =
  s |> Sexp.of_string |> response_of_sexp

(*[check_victory st sq] checks if [st] is a state where the player 
 *with square type sq has won*)
let check_victory st sq = 
  if sq=X then match st with 
  | [X;X;X;_;_;_;_;_;_] | [_;_;_;X;X;X;_;_;_] | [_;_;_;_;_;_;X;X;X]
  | [X;_;_;X;_;_;X;_;_] | [_;X;_;_;X;_;_;X;_]  | [_;_;X;_;_;X;_;_;X]
  | [X;_;_;_;X;_;_;_;X] | [_;_;X;_;X;_;X;_;_] -> true 
  | _ -> false 
  else if sq=O then match st with   
  | [O;O;O;_;_;_;_;_;_] | [_;_;_;O;O;O;_;_;_] | [_;_;_;_;_;_;O;O;O]
  | [O;_;_;O;_;_;O;_;_] | [_;O;_;_;O;_;_;O;_]  | [_;_;O;_;_;O;_;_;O]
  | [O;_;_;_;O;_;_;_;O] | [_;_;O;_;O;_;O;_;_] -> true 
  | _ -> false 
  else failwith "invalid square type for check_victory"