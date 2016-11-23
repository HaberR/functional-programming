(* id is the identifier for the user *)
type id = string

type friends = id list

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


(* [req_header] contains the meta information
 * indicating what type of a request this is.
 * This is necessary for parsing from json *)
type req_header = Message | Login | Newroom | History | Block | Listrooms | Listusers

(* [req_content] is the specific content for the
 * request. It varies based on the type of
 * request *)
type req_content = 
  | Send of msg 
  | Register of id 
  | Openroom of chatroom
  | Retrieve of msg option
  | Ignore of id
  | Seerooms of id
  | Reqnone

(* [resp_content] is the specific content for the
 * response. It varies based on the type of
 * response *)
type resp_content = 
  | Messages of msg list
  | Chatrooms of chatroom list
  | Users of id list
  | Respnone

(* request is the type of requests that the client
 * may send to the server*)
type request = req_header * req_content

(* the type of the response *)
type response = req_header * resp_content * success

type resp_wo_head = resp_content * success


