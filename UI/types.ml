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

(*
type resp_content = 
  | Messages of msg list
  | Chatrooms of chatroom list
  | Users of id list
  | Respnone
  *)

type nn = Nothing [@@deriving sexp]

(* request is the type of requests that the client
 * may send to the server*)
type 'a request = req_header * 'a [@@deriving sexp]

(* the type of the response *)
type 'a response = req_header * 'a * success [@@deriving sexp]

type 'a resp_wo_head = 'a * success [@@deriving sexp]

let req_to_string req = 
  let subordinate = 
    match fst req with
    | Message -> sexp_of_msg
    | Login | Block | Listrooms -> sexp_of_id
    | Newroom | Listmessages -> sexp_of_chatroom
    | Listusers -> sexp_of_nn in
  req |> sexp_of_request subordinate |> Sexp.to_string


