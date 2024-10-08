
(** Cursor module for managing cursor movement in a text buffer *)

type t = { x : int; y : int }

let create_cursor = { x = 0; y = 0 }

module Util =
struct 
let is_alphanumeric = function 
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true 
    | _ -> false

  let is_not_alphanumeric c = not (is_alphanumeric c) && c != ' '

  (** Skip characters in a line until a condition is false *)
  let rec skip_chars_til_false line i f = 
    if i >= String.length line then -1
    else if f line.[i] then skip_chars_til_false line (i+1) f
    else i

  (** Skip white spaces starting from index i *)
  let skip_white_spaces line i = skip_chars_til_false line i (fun x -> x = ' ')

  (** Skip alphanumeric characters starting from index i *)
  let skip_alphanumeric line i = skip_chars_til_false line i is_alphanumeric

  (** Skip non-alphanumeric characters starting from index i *)
  let skip_non_alphanumeric line i = skip_chars_til_false line i is_not_alphanumeric  
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

module Word =
struct 
(* Word-related operations module - only 'w' for now *)
  (* TODO: REFACTORING this is very hard to read, maybe having is_alpha skip also white space would help?*)
  (** Move cursor to the start of the next word *)
  let next_word_start cursor buffer =
    let rec aux buffer row col is_new_line = 
      let max_row = Array.length buffer - 1 in 
      if row > max_row then 
        make_cursor (String.length buffer.(max_row) - 1) max_row
      else
        let line = buffer.(row) in
        if is_new_line then 
          make_cursor (Util.skip_white_spaces line col) row
        else
          let new_col = 
            if Util.is_alphanumeric line.[col] then Util.skip_alphanumeric line col
            else Util.skip_non_alphanumeric line col 
          in
          if new_col = -1 then 
            aux buffer (row + 1) 0 true
          else
            let new_col = Util.skip_white_spaces line new_col in 
            if new_col = -1 then 
              aux buffer (row + 1) 0 true
            else 
              make_cursor new_col row in
      aux buffer cursor.y cursor.x false

  let next_full_word_start cursor buffer = 
    let num_lines = Array.length buffer in 
    let skip_all line i = Util.skip_chars_til_false line i (fun c -> c <> ' ') in 
    let rec nfw_aux buffer row col = 
      if row < num_lines then 
        let new_col = 
          if col = (-1) then Util.skip_white_spaces buffer.(row) 0 
          else skip_all buffer.(row) col in 
        if new_col = (-1) then nfw_aux buffer (row+1) (-1)
        else 
          let new_col = Util.skip_white_spaces buffer.(row) new_col in 
          if new_col = (-1) then nfw_aux buffer (row+1) (-1)
          else {x = new_col; y = row}
      else 
        {x = String.length buffer.(num_lines-1); y = (num_lines-1)} in 
    nfw_aux buffer cursor.y cursor.x
end
