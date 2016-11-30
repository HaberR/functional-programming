open Core.Std

type id = string [@@deriving sexp]

type chatroom =
  {
    name : string;
    participants : id list;
  } [@@deriving sexp]

type msg = 
  {
    user : string; 
    room : chatroom;
    message : string; 
    timestamp : float
  } [@@deriving sexp]

type success = Success | Fail of string [@@deriving sexp]

(*
type req_header = 
  | Message 
  | Login 
  | Newroom 
  | Block 
  | Listrooms | Listusers | Listmessages
  *)

(* request is the type of requests that the client
 * may send to the server*)
type request = 
  | Message of msg
  | Login of id
  | Block of id * id
  | Unblock of id * id
  | Listrooms of id
  | Listmessages of id * msg option * chatroom
  | Newroom of chatroom 
  | Getroom of id * string
  | Listusers 
  | AddToRoom of id * id * string [@@deriving sexp]

type resp = 
  | Chatroom of chatroom
  | Messages of msg list
  | Chatrooms of chatroom list
  | Users of id list 
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

