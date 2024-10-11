
(** Cursor module for managing cursor movement in a text buffer *)

type t = { x : int; y : int }

let create_cursor = { x = 0; y = 0 }

module Util =
struct 
  (** Skip characters in a line until a condition is false *)
  let rec skip_chars_til_false line i f = 
    if i >= String.length line then -1
    else if f line.[i] then skip_chars_til_false line (i+1) f
    else i

  (** Skip white spaces starting from index i *)
  let skip_white_spaces line i = skip_chars_til_false line i (fun x -> x = ' ')

end 

let make_cursor x y = { x; y }

(** Move cursor left by one character *)
let move_left cursor = 
  make_cursor (max 0 (cursor.x - 1)) cursor.y

(** Move cursor right by one character *)
let move_right cursor buffer =
  let line_length = String.length buffer.(cursor.y) in
  make_cursor (min (line_length - 1) (cursor.x + 1)) cursor.y

(** Move cursor up by one line *)
let move_up cursor buffer = 
  if cursor.y > 0 then
    let new_y = cursor.y - 1 in
    let new_x = min cursor.x (String.length buffer.(new_y) - 1) in
    make_cursor new_x new_y
  else
    cursor

(** Move cursor down by one line *)
let move_down cursor buffer =
  let max_y = Array.length buffer - 1 in
  if cursor.y < max_y then
    let new_y = cursor.y + 1 in
    let new_x = min cursor.x (String.length buffer.(new_y) - 1) in
    make_cursor new_x new_y
  else
    cursor

(** Move cursor to the start of the current line *)
let start_of_line cursor = make_cursor 0 cursor.y

(** Move cursor to the end of the current line *)
let end_of_line cursor buffer = 
  if Array.length buffer > cursor.y then  
    make_cursor (String.length buffer.(cursor.y) - 1) cursor.y
  else cursor
 
(** Move cursor to the first non white space char*)
let first_nws_char cursor buffer = 
  if Array.length buffer > cursor.y then  
    make_cursor (Util.skip_white_spaces buffer.(cursor.y) 0) cursor.y
  else
    cursor

(** delete a single character in a line and move cursor if necessary *)
let remove_char_at_cursor buffer cursor = 
  let x, y = !cursor.x, !cursor.y in 
  let line = buffer.(y) in
  let len = String.length line in 
  if x > len-1 then invalid_arg "index_out_of_bound" (* this should never happen*)
  else 
    let i = x in 
    if len == 1
    then buffer.(y) <- " "
    else if len - 1 == i
      (* in this case cursor blink*) 
      then buffer.(y) <- (cursor := move_left !cursor; String.sub line 0 i)
      else buffer.(y) <- (String.sub line 0 i) ^ (String.sub line (i+1) (len - i - 1))
               
(** Print cursor position (for debugging) *)
let print_cursor cursor = 
  Printf.printf "cursor -> {x: %d, y: %d}\n" cursor.x cursor.y

(** Word-related cursor operations module *)
module Word =
struct 

  type char_t = Alhanumeric | Puctuation | WhiteSpace | Escape | NullChar

  let get_char_type = function
    'a' .. 'z' | 'A' .. 'Z' |  '0' .. '9' | '_' -> Alhanumeric
  | ' '  -> WhiteSpace
  | '\n' -> Escape
  |  _   -> Puctuation 

  (*TODO start_char_type exception on empty buffer*)
  let next_word_start starting buffer = 
    let lines = Array.length buffer in
    let start_char_type = get_char_type buffer.(starting.y).[starting.x] in
    let rec nws_aux x y last_char_type =
      if lines <= y then {x = String.length buffer.(lines-1)-1; y = lines-1}
      else if String.length buffer.(y) <= x then nws_aux 0 (y+1) Escape 
        else match last_char_type, get_char_type buffer.(y).[x] with
        | Alhanumeric, Puctuation | (Escape | WhiteSpace), (Puctuation | Alhanumeric) | Puctuation, Alhanumeric -> {x; y}
        | _ , (_ as cty) -> nws_aux (x+1) y cty 
    in nws_aux (starting.x) starting.y start_char_type
  
  let next_full_word_start starting buffer = 
    let lines = Array.length buffer in
    let start_char_type = get_char_type buffer.(starting.y).[starting.x] in
    let rec nfws_aux x y last_char_type =
      if lines <= y then {x = String.length buffer.(lines-1)-1; y = lines-1}
      else if String.length buffer.(y) <= x then nfws_aux 0 (y+1) Escape 
        else match last_char_type, get_char_type buffer.(y).[x] with
        | (WhiteSpace | Escape), (Alhanumeric | Puctuation) -> {x; y}
        | _ , (_ as cty) -> nfws_aux (x+1) y cty 
    in nfws_aux (starting.x) starting.y start_char_type

    let next_word_end starting buffer = 
      let lines = Array.length buffer in
      let _start_char_ty = get_char_type buffer.(starting.y).[starting.x] in
      let rec nwe_aux x y last_ct =
        if lines <= y then {x = String.length buffer.(lines-1)-1; y = lines-1}
          else
          let current_ct = if x < String.length buffer.(y) then get_char_type buffer.(y).[x] else Escape
          in match (last_ct, current_ct) with
            | Alhanumeric, Puctuation 
            | Puctuation, Alhanumeric 
            | (Alhanumeric | Puctuation), (WhiteSpace | Escape) -> {x = (x-1); y}
            | (NullChar | WhiteSpace) , Escape -> nwe_aux 0 (y+1) Escape
            | _, (_ as cty) -> nwe_aux (x+1) y cty
      in nwe_aux (starting.x+1) starting.y NullChar

      let next_full_word_end starting buffer = 
        let lines = Array.length buffer in
        let rec nwe_aux x y last_ct =
          if lines <= y then {x = String.length buffer.(lines-1)-1; y = lines-1}
            else
            let current_ct = if x < String.length buffer.(y) then get_char_type buffer.(y).[x] else Escape
            in match (last_ct, current_ct) with
              | (Alhanumeric | Puctuation), Escape   
              | (Alhanumeric | Puctuation), WhiteSpace -> {x = (x-1); y}
              | (NullChar | WhiteSpace), Escape -> nwe_aux 0 (y+1) Escape
              | _, (_ as cty) -> nwe_aux (x+1) y cty 
        in nwe_aux (starting.x+1) starting.y NullChar
      
      let word_start_backwards starting buffer =
        let rec wsb_aux x y last_ct =  
          let current_ct = if x >= 0 then get_char_type buffer.(y).[x] else Escape in
          match current_ct, last_ct with
          | Puctuation, Alhanumeric
          | Alhanumeric, Puctuation
          | WhiteSpace, (Alhanumeric | Puctuation) -> {x = (x+1); y}
          | Escape, (Puctuation | Alhanumeric) -> {x = 0; y}
          | Escape, _ when y > 0 -> wsb_aux (String.length buffer.(y-1)-1) (y-1) Escape
          | Escape, _ -> {x = 0; y = 0}
          | _, _ -> wsb_aux (x-1) y current_ct 
        in wsb_aux (starting.x-1) starting.y NullChar 


end
