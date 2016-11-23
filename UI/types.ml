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

type success = bool [@@deriving sexp]

type req_header = 
  | Message 
  | Login 
  | Newroom 
  | Block 
  | Listrooms | Listusers | Listmessages [@@deriving sexp]

(* request is the type of requests that the client
 * may send to the server*)
type request = 
  | Message of msg
  | Login of id
  | Block of id
  | Listrooms of id
  | Listmessages of chatroom
  | Newroom of chatroom 
  | Listusers [@@deriving sexp]

type resp = int [@@deriving sexp]

(* the type of the response *)
type response = resp * success [@@deriving sexp]

let req_to_string req = 
  req |> sexp_of_request |> Sexp.to_string

let req_from_string s =
  s |> Sexp.of_string |> request_of_sexp
