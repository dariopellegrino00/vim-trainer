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

let%test_unit "e_jump_inline" = 
    [%test_eq: int] (Word.next_word_end (make_cursor 0 0) [| "Hello World!"|]).x 4;
    [%test_eq: int] (Word.next_word_end (make_cursor 0 0) [| " World!"|]).x 5;
    [%test_eq: int] (Word.next_word_end (make_cursor 3 0) [| "word!word"|]).x 4;
    [%test_eq: int] (Word.next_word_end (make_cursor 2 0) [| "wor_d"|]).x 4;
    [%test_eq: int] (Word.next_word_end (make_cursor 4 0) [| "space  "|]).x 6;
    [%test_eq: int] (Word.next_word_end (make_cursor 0 0) [| "2"|]).x 0;
    [%test_eq: int] (Word.next_word_end (Word.next_word_end (make_cursor 0 0) [| "a       !2"|]) [| "a       !2"|]).x 9

let%test_unit "e_jump_row_base" = 
    let row_jumped = (Word.next_word_end (make_cursor 4 0) [| "Hello"; "Worlds!"|]) in 
    [%test_eq: int] row_jumped.y 1;
    [%test_eq: int] row_jumped.x 5

let%test_unit "e_jump_row_advanced" = 
    let row_jumped = 
        Word.next_word_end (Word.next_word_end (make_cursor 4 0) [| "Hello!"; " ***Worlds!"|]) [| "Hello!"; " ***Worlds!"|] in 
    [%test_eq: int] row_jumped.y 1;
    [%test_eq: int] row_jumped.x 3

let%test_unit "e_jump_some_space_rows" = 
    let row_jumped = (Word.next_word_end (make_cursor 4 0) [| "Hello  ";"  "; "  ds!"|]) in 
        [%test_eq: int] row_jumped.y 2;
        [%test_eq: int] row_jumped.x 3

let%test_unit "E_jump_inline" = 
    [%test_eq: int] (Word.next_full_word_end (make_cursor 0 0) [| "Hello World!"|]).x 4;
    [%test_eq: int] (Word.next_full_word_end (make_cursor 0 0) [| " World!"|]).x 6;
    [%test_eq: int] (Word.next_full_word_end (make_cursor 3 0) [| "word!word"|]).x 8;
    [%test_eq: int] (Word.next_full_word_end (make_cursor 2 0) [| "wor_d"|]).x 4;
    [%test_eq: int] (Word.next_full_word_end (make_cursor 0 0) [| "2"|]).x 0;
    [%test_eq: int] (Word.next_full_word_end (make_cursor 0 5) [| "spaces  "|]).x 7;
    [%test_eq: int] (Word.next_full_word_end (Word.next_word_end (make_cursor 0 0) [| " a       !2"|]) [| " a       !2"|]).x 10

let%test_unit "E_jump_row_base" = 
    let row_jumped = (Word.next_full_word_end (make_cursor 4 0) [| "Hello"; "Worlds!"|]) in 
    [%test_eq: int] row_jumped.y 1;
    [%test_eq: int] row_jumped.x 6

let%test_unit "E_jump_row_advanced" =  
    let row_jumped = 
        Word.next_full_word_end (Word.next_full_word_end (make_cursor 4 0) [| "Hello!"; " ***Worlds!"|]) [| "Hello!"; " ***Worlds!"|] in 
    [%test_eq: int] row_jumped.y 1;
    [%test_eq: int] row_jumped.x 10 

let%test_unit "E_jump_some_space_rows" = 
    let row_jumped = (Word.next_full_word_end (make_cursor 4 0) [| "Hello  ";"  "; "  ds! "|]) in 
        [%test_eq: int] row_jumped.y 2;
        [%test_eq: int] row_jumped.x 4 

(* TESTING b and B commands in normal mode*)


let%test_unit "b_inline_movs" = 
    let c1 = (Word.word_start_backwards (make_cursor 0 0) [| " "|]) in 
    let c2 = (Word.word_start_backwards (make_cursor 7 0) [| "word   a"|]) in 
    let c3 = (Word.word_start_backwards (make_cursor 6 0) [| " ??   a"|]) in 
    [%test_eq: int] c1.x 0;
    [%test_eq: int] c1.y 0;
    [%test_eq: int] c2.x 0; 
    [%test_eq: int] c2.y 0;
    [%test_eq: int] c3.x 1;
    [%test_eq: int] c3.y 0

let%test_unit "b_jump_space_rows_back" =
    let jump_spaces = Word.word_start_backwards (make_cursor 2 4) [| "oi"; "  "; " "; "  "; "  word"|] in
    [%test_eq: int] jump_spaces.x 0; 
    [%test_eq: int] jump_spaces.y 0