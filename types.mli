(* msg is the type of a message sent by the user to another
 * user *)
type msg = 
  {
    user : string; 
    recipients : string list;
    message : string; 
    timestamp : float
  }

(* id is the identifier for the user *)
type id = string

(* indicates whether a request was successful *)
type success = bool

(* [resp_header] contains the meta information
 * indicating what type of a response this is.
 * This is necessary for parsing from json *)
type resp_header = Message | Login

(* [resp_content] is the specific content for the
 * response. It varies based on the type of
 * response *)
type resp_content = Received of msg | Authenticated

(* [req_header] contains the meta information
 * indicating what type of a request this is.
 * This is necessary for parsing from json *)
type req_header = Message | Login

(* [req_content] is the specific content for the
 * request. It varies based on the type of
 * request *)
type req_content = Send of msg | Login of id

(* request is the type of requests that the client
 * may send to the server*)
type request = req_header * req_content

(* the type of the response *)
type response = resp_header * resp_content * success

(* [to_x str] takes a string [str] and converts
 * it to type [x] if it is possible to do so.
 * Otherwise, it raises an exception. *)

val to_request : string -> request

val to_response : string -> response

