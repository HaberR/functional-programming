
module type Handler = sig

  val handle_request : Type_info.request -> Type_info.response

end
