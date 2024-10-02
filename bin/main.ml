open Raylib
open Cursor 

let setup () =
  Raylib.init_window 800 450 "vimtrainer";
  Raylib.set_target_fps 60;
  set_trace_log_level Fatal (* Show only warnings *)


(* text buffer definition*)
let buffer = 
  [| 
    "Hello, world!"; 
    "This is a simple text editor"; 
    "with Vim hjkl movements."; 
    "delete all the x to win";
    "*!**a magnum";
    "  x   xxx";
    "x ";
    "x    x x";
    "   x ";
    "              x";
  |]

let cursor = ref create_cursor
let font_size = 16  
let char_width = 8  
let char_height = 16 
let start_blink_time = ref (get_time ())
let blink = ref true 
let blink_delay = 0.5

let cursor_moving = ref false

(* distance of lines from line number*)
let line_offset = 3

(*draw line numbers*)
let draw_line_numbers buffer font =
  Array.iteri(fun i _ ->
    draw_text_ex font (string_of_int i) (Vector2.create (10. +. float_of_int (0 * char_width)) (10. +. float_of_int (i * char_height))) (float_of_int font_size) 1.0 Color.yellow; 
  ) buffer

(* buffer visualization*)
let draw_buffer buffer cursor font blink =
  Array.iteri (fun i line -> 
    for j = 0 to String.length line - 1 do    
      if i = cursor.y && j = cursor.x then
        (* draw the cursor in red *) 
        if blink then draw_rectangle (10 * line_offset + j * char_width) (10 + i * char_height) char_width char_height Color.red; 
      
      (* draw a single character with given font*)
      draw_text_ex font (String.make 1 line.[j]) (Vector2.create (10. *. (float_of_int line_offset) +. float_of_int (j * char_width)) (10. +. float_of_int (i * char_height))) (float_of_int font_size) 1.0 Color.white;
    done
  ) buffer

(* delete a single character in a line*)
let remove_char_at_cursor buffer cursor = 
  let x, y = !cursor.x, !cursor.y in 
  let line = buffer.(y) in
  let len = String.length line in 
  if x > len-1 then invalid_arg "index_out_of_bound" (* this should never happen*)
  else 
    let i = x in 
    if len == 1
    then buffer.(y) <- " "
    else 
      if len - 1 == i
      (* in this case cursor blink*) 
      then buffer.(y) <- (cursor := move_left !cursor; cursor_moving := true; String.sub line 0 i)
      else buffer.(y) <- (String.sub line 0 i) ^ (String.sub line (i+1) (len - i - 1))

let is_moving_key keys = 
  let rec maux acc = function
  | k :: ks ->  maux (acc || is_key_pressed k) ks
  | [] -> acc
  in maux false keys  

let movement_keys = Key.H :: Key.L :: Key.K :: Key.J :: Key.W :: []

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    (* cursor blink*)
    let current_t = get_time () in
    if abs_float (current_t -. !start_blink_time) >= blink_delay then (
      blink := not !blink; 
      start_blink_time := current_t;  
    );

    if (is_moving_key movement_keys)
      then cursor_moving := true;    

    (*movements *)
    if is_key_pressed Key.L then cursor := move_right !cursor buffer;
    if is_key_pressed Key.H then cursor := move_left !cursor;
    if is_key_pressed Key.K then cursor := move_up !cursor buffer;
    if is_key_pressed Key.J then cursor := move_down !cursor buffer;
    (* this is garanteed working at all*)
    if is_key_pressed Key.W then cursor := next_word_start !cursor buffer;

    (* editing*)
    if is_key_pressed Key.X then remove_char_at_cursor buffer cursor;

    if !cursor_moving then ((blink := true; start_blink_time := current_t;); cursor_moving := false); 

    begin_drawing ();
    clear_background Color.black;

    let font = load_font "resources/DejaVuSansMono.ttf" in 
    draw_buffer buffer !cursor font !blink;
    draw_line_numbers buffer font;

    end_drawing ();
    loop ()

let () = setup () |> loop