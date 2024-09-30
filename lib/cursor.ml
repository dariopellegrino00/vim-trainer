
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