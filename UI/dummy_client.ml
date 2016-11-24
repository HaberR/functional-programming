open Type_info
open Lwt

let init () = ()

let send_req req =
  let info = req_to_string req in
  print_string info;
  return ((match req with
  | Message _ | Login _ | Block _ | Newroom _ -> Nothing
  | Listusers -> Users ["default 1"; "default 2"]
  | Listrooms _ -> Chatrooms [{ name = "chat room"; participants = ["default 2"]}]
  | Listmessages _ -> Messages [
    { 
      user = "default 0"; 
      room = { 
        name = "chat room";
        participants = ["default 2"]
      };
      message = "hi";
      timestamp = 1.24939
    }]
  | Getroom _ -> Chatroom { name = "chat room"; participants = ["default 2"]}), true)
