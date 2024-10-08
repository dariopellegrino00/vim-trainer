open Raylib
open Cursor 
open Drawer
open NormalModeParser

let setup () =
  Raylib.init_window 400 350 "vimtrainer";
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

(**normal mode motion command evaluator*)
let eval_motion_cmd cmd cursor buffer = 
  match cmd with 
  |  MotionOnly (_, c) -> (
    match c with 
    | CharRight   -> cursor := move_right !cursor buffer
    | CharLeft    -> cursor := move_left !cursor
    | LineUp      -> cursor := move_up !cursor buffer
    | LineDown    -> cursor := move_down !cursor buffer
    | StartOfLine -> cursor := start_of_line !cursor
    | EndOfLine   -> cursor := end_of_line !cursor buffer
    | BeginOfLine -> cursor := first_nws_char !cursor buffer
    | NextWordStart -> cursor := Word.next_word_start !cursor buffer
    | NextFullWordStart -> cursor := Word.next_full_word_start !cursor buffer
(*|  -> remove_char_at_cursor buffer cursor (*TODO refactor params order*) *)
    | NotAMotion -> ()
  )
  | SimleOperation c -> (
    match c with 
    | "x" -> remove_char_at_cursor buffer cursor
    | _   -> () 
  )

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
      let string_cmd = String.make 1 (Uchar.to_char input_key) in 
      let command = NormalModeParser.parse_command string_cmd in 
      eval_motion_cmd command cursor buffer
    );
   
    begin_drawing ();
    clear_background Color.darkgray;
    
    let font = load_font "resources/DejaVuSansMono.ttf" in 

    draw_line_numbers buffer font;
    draw_cursor !cursor.x !cursor.y !blink; (* draw the cursor first to not overwrite the character below*)
    draw_buffer buffer font;

    end_drawing ();
    loop ()

let () = setup () |> loop