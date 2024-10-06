(** Cursor module for managing cursor movement in a text buffer *)

type t = { x : int; y : int }

let create_cursor = { x = 0; y = 0 }

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
  make_cursor (String.length buffer.(cursor.y) - 1) cursor.y

(** Print cursor position (for debugging) *)
let print_cursor cursor = 
  Printf.printf "cursor -> {x: %d, y: %d}\n" cursor.x cursor.y

(** Word-related operations module - only 'w' for now *)
module Word = struct
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
          make_cursor (skip_white_spaces line col) row
        else
          let new_col = 
            if is_alphanumeric line.[col] then skip_alphanumeric line col
            else skip_non_alphanumeric line col 
          in
          if new_col = -1 then 
            aux buffer (row + 1) 0 true
          else
            let new_col = skip_white_spaces line new_col in 
            if new_col = -1 then 
              aux buffer (row + 1) 0 true
            else 
              make_cursor new_col row 
    in
    aux buffer cursor.y cursor.x false
end