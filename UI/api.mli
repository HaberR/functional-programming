open Type_info
open Lwt
(* functions in this file will be called
 * in order to make requests and get the responses in
 * an abstract, UI independent form. The front end is
 * responsible for displaying the responses. In general,
 * the success of the response is given back to the client
 * only if there is some legitimate reason that the request
 * could fail (ie. a login attempt has the wrong password)
 *)


module type Requester = sig 

  (*[login uid] sends a login request and returns
   * a success indicating the success of the action*)
  val login : id -> success Lwt.t

  (*[register uid pass] attempts to register [uid]
   * with password [pass] and returns a success
   * status *)
  val register: id -> string-> success Lwt.t

  val auth : id -> string-> success Lwt.t

  (*[see_chatrooms uid] gives back the list of chatrooms
   * the user [uid] has access to. Since this activity
   * should always be successful, there is no success report*)
  val see_chatrooms : id -> Type_info.chatroom list Lwt.t

  (*[see_users ()] provides a list of all the users
   * who are registered on the server*)
  val see_users : unit -> id list Lwt.t

  (*[see_messages uid last cr] is the list of all messages in
   * chatroom cr which have shown up since message [last]. If
   * [last] is none, this is simply the entire message history*)
  val see_messages : id -> Type_info.msg option -> Type_info.chatroom -> msg list Lwt.t

  (*[block_user u t] sends a request to block the
   * user [t]*)
  val block_user : id -> id -> success Lwt.t

  (*[unblock_user u t] sends a request to unblock
   * the user [t]*)
  val unblock_user : id -> id -> success Lwt.t

  (*[send_message uid cr cont] posts a message to the server
   * and provides the posted message in response (so the
   * client side can keep it for its records*)
  val send_message : id -> Type_info.chatroom -> string -> (Type_info.msg * success) Lwt.t

  (*[get_room uid crname] is the room with name [crname]
   * provided that the user with name [crname] has access
   * to that room.*)
  val get_room : id -> string -> (Type_info.chatroom * success) Lwt.t

  (*[new_room plist crname] creates a new room 
   * with participants [plist] and name [crname] and
   * indicates the success of the action *)
  val new_room : id list -> string -> success Lwt.t

  (*[new_game members gname] sends a Newgame request for a new game
   *with name [gname] and members [members]. Gets Nothing as response*)
  val new_game : id list -> string -> success Lwt.t 

  (*[see_games id] sends a Listgames request with [id] attached and gets back
   *a Gamerooms response with the list of games the user represented by [id]
   *is in*)
  val see_games : id -> Type_info.gameroom list Lwt.t 

  (*[get_game id grname] sends a Getgame request and gets back either a
   *Gamestate response if the user is a player in the game or Nothing if not.*)
  val get_game : id -> string -> ((Type_info.gameroom * Type_info.square list) * success) Lwt.t 

  (*[add_user_to_room u a crname] adds user [a] to room with
   * name [crname] provided that u has access to said room.*)
  val add_user_to_room : id -> id -> string -> success Lwt.t

  (*[leave_room u rmname] removes [u] from the participants
   * in the room with name [rmname] and indicates the success*)
  val leave_room : id -> string -> success Lwt.t

  (*[fill_board id gr sq_num] sends a Changegamest request to change the square
   *represented by [sq_num] in game [gr] to X or O depending on [id]. Gets
   *Nothing as a response.*)
  val fill_board : id -> Type_info.gameroom -> int -> success Lwt.t
  
  (*[reset_board id gr] sends a Resetgame request to reset the game [gr] 
   *Gets Nothing as a response.*)
  val reset_board : id -> Type_info.gameroom -> success Lwt.t 
  
  (*[getwl id] sends a Getwl request for the user represented by [id]. Gets
   *Wl as a response containing the win-loss information.*)
  val getwl : id -> (int*int) Lwt.t 
end

module type RequesterMaker =
 functor (Cl : Chat_client.Client ) -> Requester

module MakeRequester : RequesterMaker
