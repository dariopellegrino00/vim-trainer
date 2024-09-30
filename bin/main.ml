open Raylib
open Cursor 

let setup () =
  Raylib.init_window 800 450 "vimtrainer";
  Raylib.set_target_fps 60;
  set_trace_log_level Fatal (* Show only warnings *)


(* text buffer definition*)
let buffer = [| "Hello, world!"; "This is a simple text editor"; "with Vim hjkl movements."; |] 
let cursor = ref create_cursor
let font_size = 16  
let char_width = 8  
let char_height = 16 
let start_blink_time = ref (get_time ())
let blink = ref true 
let blink_delay = 0.5

(* buffer visualization*)
let draw_buffer buffer cursor font blink =
  Array.iteri (fun i line ->
    for j = 0 to String.length line - 1 do
      if i = cursor.y && j = cursor.x then
        (* draw the cursor in red *) 
        if blink then draw_rectangle (10 + j * char_width) (10 + i * char_height) char_width char_height Color.red;

      (* draw a single character with given font*)
      draw_text_ex font (String.make 1 line.[j]) (Vector2.create (10. +. float_of_int (j * char_width)) (10. +. float_of_int (i * char_height))) (float_of_int font_size) 1.0 Color.white;
    done
  ) buffer

(* delete a single character in a line*)
let remove_char_at_cursor buffer cursor = 
  let line = buffer.(cursor.y) in
  let len = String.length line in 
  if cursor.x > len-1 then invalid_arg "index_out_of_bound" (* this should never happen*)
  else 
    let i = cursor.x in 
    if len == 1 
      then buffer.(cursor.y) <- " " 
    else 
    buffer.(cursor.y) <- (String.sub line 0 i) ^ (String.sub line (i+1) (len - i - 1))


let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let current_t = get_time () in
    if abs_float (current_t -. !start_blink_time) >= blink_delay then (
      blink := not !blink; 
      start_blink_time := current_t;  
    );

    if is_key_pressed Key.H || is_key_pressed Key.L || is_key_pressed Key.K || is_key_pressed Key.J 
      then (blink := true; start_blink_time := current_t;); 

    (*movements *)
    if is_key_pressed Key.H then cursor := move_left !cursor;
    if is_key_pressed Key.L then cursor := move_right !cursor buffer;
    if is_key_pressed Key.K then cursor := move_up !cursor buffer;
    if is_key_pressed Key.J then cursor := move_down !cursor buffer;

    (* editing*)
    if is_key_pressed Key.X then remove_char_at_cursor buffer !cursor;

    begin_drawing ();
    clear_background Color.black;

    let font = load_font "resources/DejaVuSansMono.ttf" in 
    draw_buffer buffer !cursor font !blink;

    end_drawing ();
    loop ()

let () = setup () |> loop