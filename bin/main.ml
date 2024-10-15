open Raylib
open Cursor 
open Drawer
open NormalModeParser
open NormalModeEvaluator
open TextBuffer

let setup () =
  Raylib.init_window 400 350 "vimtrainer";
  Raylib.set_target_fps 60; (*TODO: refactor time based on fps*)
  set_trace_log_level Fatal (* Show only warnings *)

let completed_level f buffer = Array.for_all f buffer

let load_level1 = 
  let level1 =  lines "resources/levels/level1.txt" in 
  list_to_array level1
let completed_level1 buffer = completed_level (fun line -> (String.for_all (fun c -> c <> 'x') line)) buffer 

(* example text buffer definition*)
let buffer = load_level1

(*for cursor logic and visuals*)
let cursor = ref create_cursor
let cursor_old = ref !cursor
(*cursor blink logic*)
let blink = ref true 
let blink_delay = 0.5
let start_blink_time = ref (get_time ())
let string_cmd = ref ""

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    (* cursor blink logic*)
    let current_t = get_time () in
    if abs_float (current_t -. !start_blink_time) >= blink_delay then (
      blink := not !blink; 
      start_blink_time := current_t;  
    );

    (* has cursor moved? if yes blink*)
    if !cursor_old <> !cursor then ((blink := true; start_blink_time := current_t));
    cursor_old := !cursor;

    let input_key = get_char_pressed () in 
    if Uchar.to_char input_key != (char_of_int 0) then (
      let key = String.make 1 (Uchar.to_char input_key) in 
      string_cmd := !string_cmd ^ key;
      let command = NormalModeParser.parse_command !string_cmd in 
      match command with 
        NotACommand -> string_cmd := "" 
      | Partial _   -> () 
      | _           -> string_cmd := ""; eval_motion_cmd command cursor buffer
    );
    
    
    begin_drawing ();
    clear_background Color.darkgray;
    
    let font = load_font "resources/DejaVuSansMono.ttf" in 

    draw_line_numbers buffer font;
    draw_cursor !cursor.x !cursor.y !blink; (* draw the cursor first to not overwrite the character below*)
    draw_buffer buffer font;

    end_drawing ();
    if not (completed_level1 buffer) then loop ()
    

let () = setup () |> loop