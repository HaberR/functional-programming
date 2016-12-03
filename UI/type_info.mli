(* id is the identifier for the user *)
type id = string

(*square is a type representing the state of a tic-tac-toe square
 *N represents an empty square*)
type square = N | X | O 

type chatroom =
  {
    name : string;
    participants : id list;
  }

type gameroom = 
  {
    name : string ; 
    players : id list ;
  }

(* msg is the type of a message sent by the user to another
 * user *)
type msg = 
  {
    user : string; 
    room : chatroom;
    message : string; 
    timestamp : float
  }

(* [msg_hist] is the type used to request the message history
 * of a chatroom. the timestamp represents the time of the last
 * message the client has on record 
type get_msgs =
  {
    user : string;
    room : chatroom;
    timestamp : float option;
  }*)

(* indicates whether a request was successful *)
type success = Success | Fail of string

(* [resp_header] contains the meta information
 * indicating what type of a response this is.
 * This is necessary for parsing from json 
type resp_header = Message | Login*)

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
type response = resp * success

val req_to_string : request -> string

val resp_to_string : response -> string

val req_from_string : string -> request

val resp_from_string : string -> response

val check_victory : square list -> square -> bool