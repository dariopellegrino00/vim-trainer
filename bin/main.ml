open Raylib
open Cursor 

let setup () =
  Raylib.init_window 800 450 "vimtrainer";
  Raylib.set_target_fps 60; (*TODO: refactor time based on fps*)
  set_trace_log_level Fatal (* Show only warnings *)


(* example text buffer definition*)
let buffer = 
  [| 
    "Hello, world!"; 
    "This is a simple text editor"; 
    "with Vim hjkl movements."; 
    "delete all the x to win ! ! !";
    "*!**a magnum ! ";
    "  x   xxx";
    "x ";
    "x    x x";
    "   x ";
    "              x";
  |]

(*for cursor logic and visuals*)
let cursor = ref create_cursor
let cursor_old = ref !cursor
(*cursor blink logic*)
let blink = ref true 
let blink_delay = 0.5
let start_blink_time = ref (get_time ())
(*cursor repeaed moves logic*)
let last_move_time = ref (get_time ())
let key_down_time = ref (0.)
(*last frame pressed key*)
let pressed_key = ref (get_key_pressed ())

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

(**normal mode key-action binding*)
let normal_mode_action key cursor buffer = 
  match key with 
  | Key.L -> cursor := move_right !cursor buffer
  | Key.H -> cursor := move_left !cursor
  | Key.K -> cursor := move_up !cursor buffer
  | Key.J -> cursor := move_down !cursor buffer
  | Key.W -> cursor := next_word_start !cursor buffer
  | Key.X -> remove_char_at_cursor buffer cursor (*refactor params order*)
  | Key.Zero -> cursor := goto_start_of_line !cursor
  | _ -> ()

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    (* cursor blink*)
    let current_t = get_time () in
    if abs_float (current_t -. !start_blink_time) >= blink_delay then (
      blink := not !blink; 
      start_blink_time := current_t;  
    );
    
    (*if key last key was release stop moving*)
    if is_key_released !pressed_key 
      then pressed_key := Null
    else 
      if abs_float(get_time () -. !key_down_time) > 1. (*repeat key down after 1 sec*)
        then if abs_float (current_t -. !last_move_time ) >= 0.1 (* TODO refactor time in base al frame rate?*)
        then normal_mode_action !pressed_key cursor buffer;

    (* if pressed get the current pressed key and reset the down key time*)
    let current_key = get_key_pressed () in 
    if current_key != Null then (key_down_time := current_t; pressed_key := current_key);
    normal_mode_action current_key cursor buffer;

    (* has cursor moved? if yes blink*)
    if !cursor_old <> !cursor then ((blink := true; start_blink_time := current_t));
    cursor_old := !cursor;
    
    begin_drawing ();
    clear_background Color.darkgray;

    let font = load_font "resources/DejaVuSansMono.ttf" in 

    draw_line_numbers buffer font;
    draw_cursor cursor !blink; (* draw the cursor first to not overwrite the character below*)
    draw_buffer buffer font;

    end_drawing ();
    loop ()

let () = setup () |> loop