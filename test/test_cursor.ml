open Cursor
open Base 

(*vim w unit tests*)
let%test_unit "w_jump_on_second_row" = 
    let row_jumped = (Word.next_word_start (make_cursor 6 0) [| "  a row"; " another row"|]) in
    [%test_eq: int] row_jumped.x 1;
    [%test_eq: int] row_jumped.y 1

let%test_unit "w_jump_on_second_row" = 
    let row_jumped = (Word.next_word_start (make_cursor 6 0) [| "  a row"; "!another row"|]) in
    [%test_eq: int] row_jumped.x 0;
    [%test_eq: int] row_jumped.y 1

let%test_unit "w_skip_ws_rows" =
    let buffer = [| "  a row "; "  "; "   "; "  s "; " "; " ?"|] in 
    let first_rows_jumped = (Word.next_word_start (make_cursor 6 0) buffer) in 
    let last_rows_jumped = (Word.next_word_start first_rows_jumped buffer) in 
    [%test_eq: int] first_rows_jumped.x 2;
    [%test_eq: int] first_rows_jumped.y 3;
    [%test_eq: int] last_rows_jumped.x 1;
    [%test_eq: int] last_rows_jumped.y 5

let%test_unit "w_jump_inline" = 
    let starting_pos = make_cursor 0 0 in  
    [%test_eq: int] (Word.next_word_start starting_pos [| "fullword anotherfullword"|]).x 9;
    [%test_eq: int] (Word.next_word_start starting_pos [| "   er4"|]).x 3;
    [%test_eq: int] (Word.next_word_start starting_pos [| "! a"|]).x 2;
    [%test_eq: int] (Word.next_word_start starting_pos [| " !  "|]).x 1

let%test_unit "W_jump_on_second_row_first_char" = 
    let row_jumped = (Word.next_full_word_start (make_cursor 6 0) [| "  a row"; "another row?"|]) in
    [%test_eq: int] row_jumped.x 0;
    [%test_eq: int] row_jumped.y 1

(* vim W unit tests *)
let%test_unit "W_jump_on_second_row_space" = 
    let row_jumped = (Word.next_full_word_start (make_cursor 6 0) [| " a row!"; " ?another row?"|]) in
    [%test_eq: int] row_jumped.x 1;
    [%test_eq: int] row_jumped.y 1

let%test_unit "W_skip_ws_rows" =
    let buffer = [| " a row!"; "  "; "   "; "  !s "; " "; " ?"|] in 
    let first_rows_jumped = (Word.next_full_word_start (make_cursor 4 0) buffer) in 
    let last_rows_jumped = (Word.next_full_word_start first_rows_jumped buffer) in 
    [%test_eq: int] first_rows_jumped.x 2;
    [%test_eq: int] first_rows_jumped.y 3;
    [%test_eq: int] last_rows_jumped.x 1;
    [%test_eq: int] last_rows_jumped.y 5

let%test_unit "W_jump_inline" = 
    let starting_pos = make_cursor 0 0 in  
    [%test_eq: int] (Word.next_full_word_start starting_pos [| "fullword! anotherfullword!"|]).x 10;
    [%test_eq: int] (Word.next_full_word_start starting_pos [| "   er4"|]).x 3;
    [%test_eq: int] (Word.next_full_word_start starting_pos [| "! a"|]).x 2;
    [%test_eq: int] (Word.next_full_word_start starting_pos [| " !  "|]).x 1

