open Raylib
open Cursor 

let setup () =
  Raylib.init_window 800 450 "vimtrainer";
  Raylib.set_target_fps 60;
  set_trace_log_level Fatal (* Show only warnings *)


(* example text buffer definition*)
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

(*for cursor logic and visuals*)
let cursor = ref create_cursor
let cursor_old = ref !cursor
let blink = ref true 
let blink_delay = 0.5
let start_blink_time = ref (get_time ())

(*for text size and visualization*)
let font_size = 16  
let char_width = 8  
let char_height = 16 

(* distance between lines and corresponding line number*)
let line_offset = 3

(*draw line numbers*)
let draw_line_numbers buffer font =
  Array.iteri(fun i _ ->
    draw_text_ex font (string_of_int i) (Vector2.create (10. +. float_of_int (0 * char_width)) (10. +. float_of_int (i * char_height))) (float_of_int font_size) 1.0 Color.yellow; 
  ) buffer

(* draw cursor with blink*)
let draw_cursor cursor blink = 
  if blink then draw_rectangle (10 * line_offset + !cursor.x * char_width) (10 + !cursor.y * char_height) char_width char_height Color.red

(* buffer visualization*)
let draw_buffer buffer font =
  Array.iteri (fun i line -> 
    for j = 0 to String.length line - 1 do    
      (* draw a single character with given font*)
      draw_text_ex font (String.make 1 line.[j]) (Vector2.create (10. *. (float_of_int line_offset) +. float_of_int (j * char_width)) (10. +. float_of_int (i * char_height))) (float_of_int font_size) 1.0 Color.white;
    done
  ) buffer

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

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    (* cursor blink*)
    let current_t = get_time () in
    if abs_float (current_t -. !start_blink_time) >= blink_delay then (
      blink := not !blink; 
      start_blink_time := current_t;  
    );

    (*basic movements *)
    if is_key_pressed Key.L then cursor := move_right !cursor buffer;
    if is_key_pressed Key.H then cursor := move_left !cursor;
    if is_key_pressed Key.K then cursor := move_up !cursor buffer;
    if is_key_pressed Key.J then cursor := move_down !cursor buffer;

    (* jumping movements*)
    if is_key_pressed Key.W then cursor := next_word_start !cursor buffer;
    if is_key_pressed Key.Zero then cursor := goto_start_of_line !cursor;

    (* editing*)
    if is_key_pressed Key.X then remove_char_at_cursor buffer cursor;

    (* has cursor moved?*)
    if !cursor_old <> !cursor then ((blink := true; start_blink_time := current_t));
    cursor_old := !cursor;
    
    begin_drawing ();
    clear_background Color.black;

    let font = load_font "resources/DejaVuSansMono.ttf" in 

    draw_line_numbers buffer font;
    draw_cursor cursor !blink; (* draw the cursor first to not overwrite the character below*)
    draw_buffer buffer font;

    end_drawing ();
    loop ()

let () = setup () |> loop