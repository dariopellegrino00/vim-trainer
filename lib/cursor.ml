
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

(* refactor in new module?? *)
let is_alphanumeric = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'-> true | _ -> false
let is_not_alphanumeric c = not (is_alphanumeric c) && c != ' '

(* TODO refactor this every method is the same change only the bool with a fun*)
(** skip white spaces starting from i char and return the first non white space idx*)
let rec skip_white_spaces line i = 
  if i >= String.length line then String.length line - 1
  else if line.[i] = ' ' then skip_white_spaces line (i+1) 
  else (print_string ("skip white "); print_char line.[i];print_int i;print_endline ""; i)  

let rec skip_alphanumeric line i =
  if i >= String.length line then String.length line - 1 
  else if is_alphanumeric line.[i] then skip_alphanumeric line (i+1) 
  else (print_string "skip alpha "; print_int i; print_endline ""; i)

let rec skip_non_alphanumeric line i = 
  if i >= String.length line then String.length line - 1 
  else if is_not_alphanumeric line.[i] then skip_non_alphanumeric line (i+1) 
  else (print_string "skip non alpha "; print_int i;print_endline ""; i)

let next_word_start cursor buffer =
  let x, y= cursor.x, cursor.y in 
  let line = buffer.(y) in
  let i_start = skip_white_spaces line x in
  let newx = 
    if is_alphanumeric line.[i_start] 
    then let newi = (skip_alphanumeric line i_start) in 
      if is_not_alphanumeric line.[newi] then newi else skip_white_spaces line newi
    else let newi = (skip_non_alphanumeric line i_start) in 
      if is_alphanumeric line.[newi] then newi else skip_white_spaces line newi in
  {cursor with x = newx}

   
(*TODO PROBLEM : cant go on single char words*)

(*!**a asfafs*)

(*Hello, world! afjajfj alora.mag0num(dei) , . gnu.,_gnu !&(;%."£",faffas*)