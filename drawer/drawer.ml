open Raylib
open Cursor

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

(* draw cursor if blink*)
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
