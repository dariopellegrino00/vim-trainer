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
    "     ";
    "  ";
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
    "   ";
  |]

(*for cursor logic and visuals*)
let cursor = ref create_cursor
let cursor_old = ref !cursor
(*cursor blink logic*)
let blink = ref true 
let blink_delay = 0.5
let start_blink_time = ref (get_time ())
let string_cmd = ref ""

(**normal mode motion command evaluator*)
let eval_motion_cmd cmd cursor buffer = 
  match cmd with 
  |  MotionOnly (n, c) -> (
    try 
      let still_count = ref 0 in
      (* cant use global cursor_old here is for cursor animation logic inside main*)
      let aux_cursor_old = ref !cursor in 
      for i = 0 to n-1 do 
        (* Exit if cursor is still for some iters, to avoid repeating a lot of operations when cursor is still
        4 to be always sure that repeated movs that sometimes remain still for 1 or 2 iters like $*) 
        if !aux_cursor_old <> !cursor then still_count := 0
        else if !still_count > 4 then raise Exit else still_count := !still_count + 1;
        aux_cursor_old := !cursor; 
        match c with 
        | CharRight   -> cursor := move_right !cursor buffer
        | CharLeft    -> cursor := move_left !cursor
        | LineUp      -> cursor := move_up !cursor buffer
        | LineDown    -> cursor := move_down !cursor buffer
        | StartOfLine -> cursor := start_of_line !cursor
        | EndOfLine   -> (
          cursor := end_of_line !cursor buffer;
          if n-1-i > 0 then cursor := end_of_line (move_down !cursor buffer) buffer
        )
        | BeginOfLine -> cursor := first_nws_char !cursor buffer
        | NextWordStart     -> cursor := Word.next_word_start !cursor buffer
        | NextFullWordStart -> cursor := Word.next_full_word_start !cursor buffer
        | NextWordEnd       -> cursor := Word.next_word_end !cursor buffer
        | NextFullWordEnd   -> cursor := Word.next_full_word_end !cursor buffer
        | BackWordStart     -> cursor := Word.word_start_backwards !cursor buffer
        | BackFullWordStart -> cursor := Word.fullword_start_backwards !cursor buffer
      done
    with Exit -> print_endline "***DEBUG: exit for";()
  )
  | SimleOperation c -> (
    match c with 
    | "x" -> remove_char_at_cursor buffer cursor
    | _   -> () 
  )
  | NotACommand | Partial _-> ()

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
    loop ()

let () = setup () |> loop