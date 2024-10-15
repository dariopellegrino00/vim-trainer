open Cursor
open NormalModeParser

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