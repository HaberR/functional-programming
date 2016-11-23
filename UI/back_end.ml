open Types
open Lwt

exception WrongMode
exception ServerError
exception ClientError
exception FailedLogin

module type Client = sig

  val init : unit -> unit

  val send_req : 'a request -> resp_wo_head Lwt.t

end


module type Backend = sig 
  type state

  val login : string -> unit Lwt.t

  val see_chatrooms : unit -> chatroom list Lwt.t

  val see_users : unit -> id list Lwt.t

  val see_messsages : unit -> msg list Lwt.t

  val block_user : unit -> success Lwt.t

  val send_message : string -> success Lwt.t

  val enter_room : unit -> state Lwt.t

  val exit_room : unit -> state
end

module type BackendMaker =
 functor (Cl : Client ) -> Backend

module MakeBackend (Cl : Client) = struct

  type setting = General | Inroom of chatroom

  type state = 
      {
        mode : setting;
        user : id;
      }

  let st = ref { mode = General; user = "" }

  (* utility function to get the chatroom quickly *)
  let get_chatroom () =
    match !st.mode with
    | Inroom cr -> cr
    | General -> raise WrongMode

  (* utility function to indicate whether we are in normal
   * mode *)
  let in_normal_mode () =
    match !st.mode with
    | Inroom _ -> raise WrongMode
    | General -> ()

  (* A wrapper for response handling that raises a useless
   * error if the request was unsuccessful *)
  let handle_response f (cont, succ) =
    if succ then f cont
    else raise ServerError

  let login identifier =
    let req = (Login, identifier) in
    Cl.send_req req >|= fun (_,succ) ->
    if succ then st := { mode = !st.mode; user = identifier }
    else raise FailedLogin

  let see_chatrooms () =
    in_normal_mode ();
    let req = (Listrooms, !st.user) in
    let f = function
      | Chatrooms clst -> clst | _ -> raise ClientError in
    Cl.send_req req >|= (handle_response f)

  let see_users =
    in_normal_mode ();
    let req = (Listusers, None) in
    let f = function
      | Users lst -> lst | _ -> raise ClientError in
    Cl.send_req req >|= (handle_response f)
    
  let see_messsages = (* () -> msg list*)
    let cr = get_chatroom () in
    (*let req = (Listmessages, 
    let req = (Retrieve *)
    failwith "unimplemented"

  let block_user = (*id -> success*)
    failwith "unimplemented"

  let send_message = (*string -> success*)
    failwith "unimplemented"

  let enter_room st cr =
    st := { mode = Inroom (cr); user = !st.user }

  let exit_room st =
    st := { mode = General; user = !st.user}

end
