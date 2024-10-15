open Raylib

(*for text size and visualization*)
let font_size = 16.  
let char_width = 8.  
let char_height = 16.

(* distance between lines and corresponding line number*)
let line_offset = 3. (*TODO: this can be made dinamic based on text rows growing or shrinking*)
let text_offset = line_offset *. 2. -. 0.5

(*draw line numbers*)
let draw_line_numbers buffer font =
  Array.iteri(fun i _ ->
    let num = (string_of_int i) in 
    let count_digits = float_of_int (String.length num) in
    let pos = (
      Vector2.create 
        (10. *. (line_offset +. 1. -. count_digits))
        (10. +. float_of_int (i) *. char_height)
    ) in
    draw_text_ex font (string_of_int i) pos font_size 1.0 Color.yellow; 
  ) buffer

(* draw cursor if blink with offset from left text_offset same for lines obv*)
let draw_cursor x y blink = 
  if blink then
    let pos = ( 
      Vector2.create 
        (10. *. text_offset +. (float_of_int x) *. char_width)
        (10. +. (float_of_int y) *. char_height)
      ) in 
      draw_rectangle_v pos (Vector2.create char_width char_height) Color.red

(* buffer visualization*)
let draw_buffer buffer font =
  Array.iteri (fun i line -> 
    for j = 0 to String.length line - 1 do    
      (* draw a single character with given font with offset from left text_offset same for cursor obv*)
      let pos = (
        Vector2.create 
          (10. *. text_offset  +. float_of_int j *. char_width) 
          (10. +. float_of_int (i) *. char_height)
        ) in
      draw_text_ex font (String.make 1 line.[j]) pos font_size 1.0 Color.white;
    done
  ) buffer
