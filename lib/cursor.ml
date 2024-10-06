
type t = {x : int; y : int}

let create_cursor = {x = 0; y = 0}

let move_left cursor = 
  { cursor with x = max 0 (cursor.x - 1) }

let move_right cursor buffer =
  let line_length = String.length buffer.(cursor.y) in
  { cursor with x = min (line_length - 1) (cursor.x + 1) }

let move_up cursor buffer = 
  let m = max 0 (cursor.y - 1) in 
  if cursor.y > 0 then
    {y = m; x = min cursor.x (String.length (buffer.(cursor.y - 1)) - 1)}
  else
    {cursor with y = m}

let move_down cursor buffer =
  let m = min ((Array.length buffer) - 1) (cursor.y + 1) in 
  if ((Array.length buffer) - 1) > cursor.y then
    {y = m; x = min cursor.x (String.length (buffer.(cursor.y + 1)) - 1)}
  else
    {cursor with y = m}

(*This is all for the 'w' of normal mode*)
(* refactor in new module?? *)
let is_alphanumeric = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'-> true | _ -> false
let is_not_alphanumeric c = not (is_alphanumeric c) && c != ' '

(** skip the next*)
let rec skip_chars_til_false line i f = 
if i >= String.length line then String.length line - 1
else if f line.[i] then skip_chars_til_false line (i+1) f
else i

(** skip white spaces starting from i char till return the idx of first non white space idx*)
let skip_white_spaces line i = skip_chars_til_false line i (fun x -> x = ' ')

(** skip alphanumeric chars from i till return the idx of the first non alphanumeric char idx*)
let skip_alphanumeric line i = skip_chars_til_false line i (fun x -> is_alphanumeric x)

(** skip non alphanumeric chars from i till return the idx of the first non alphanumeric is false char idx*)
let skip_non_alphanumeric line i = skip_chars_til_false line i (fun x -> is_not_alphanumeric x)

(*TODO PROBLEM : cant go on single char words*)
let next_word_start cursor buffer =
  let x, y= cursor.x, cursor.y in 
  let line = buffer.(y) in
  let newx = 
    if is_alphanumeric line.[x] 
    then let newi = (skip_alphanumeric line x) in 
      if is_not_alphanumeric line.[newi] then newi else skip_white_spaces line newi
    else let newi = (skip_non_alphanumeric line x) in 
      if is_alphanumeric line.[newi] then newi else skip_white_spaces line newi in
  {cursor with x = newx}

(** 
  jump to start of line
  out of bound of the string can never happen here, 
  if it happen it happened before this fun. 
*)
let goto_start_of_line cursor = {cursor with x = 0}

(*for debugging reasons*)
let print_cursor cursor = 
  print_endline("cursor -> {x : " ^ string_of_int(cursor.x) ^ " ,y: " ^ string_of_int(cursor.y) ^ "}")

