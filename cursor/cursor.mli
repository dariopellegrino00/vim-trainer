
type t = {x : int; y : int} 

val create_cursor : t 
val make_cursor : int -> int -> t
val move_left : t -> t
val move_right : t -> string array -> t
val move_up : t -> string array -> t
val move_down : t -> string array -> t
val start_of_line : t -> t
val end_of_line : t -> string array -> t 
val first_nws_char : t -> string array -> t
val print_cursor : t -> unit
val remove_char_at_cursor: string array -> t ref -> unit

module Word : sig
  val next_word_start : t -> string array -> t
end