(* id is the identifier for the user *)
type id = string

type chatroom =
  {
    name : string;
    participants : id list;
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
type success = bool

(* [resp_header] contains the meta information
 * indicating what type of a response this is.
 * This is necessary for parsing from json 
type resp_header = Message | Login*)

(* request is the type of requests that the client
 * may send to the server*)
type request = 
  | Message of msg
  | Login of id
  | Block of id
  | Listrooms of id
  | Listmessages of chatroom
  | Newroom of chatroom
  | Getroom of string
  | Listusers [@@deriving sexp]

type resp = 
  | Chatroom of chatroom
  | Messages of msg list
  | Chatrooms of chatroom list
  | Users of id list 
  | Nothing [@@deriving sexp] 

(* the type of the response *)
type response = resp * success

val req_to_string : request -> string

val resp_to_string : response -> string

val req_from_string : string -> request

val resp_from_string : string -> response

