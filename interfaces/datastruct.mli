(* A dictionary which will be used for
 * storing all the information associated
 * with each username. Internal functions
 * are self explanatory *)

type 'a t

type key

val add : key -> 'a -> 'a t -> 'a t
  
val remove : key -> 'a t -> 'a t

val find : key -> 'a t -> 'a

val mem : key -> 'a t -> bool

val get_keys : 'a t -> key list
