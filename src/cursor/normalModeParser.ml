open Angstrom

(*normal  [count] [operation] [motion] *)

type motion = 
  | CharRight 
  | CharLeft
  | LineUp
  | LineDown
  | StartOfLine
  | BeginOfLine
  | EndOfLine
  | NextWordStart
  | NextFullWordStart
  | NextWordEnd
  | NextFullWordEnd
  | BackWordStart
  | BackFullWordStart
  | NotAMotion

type operation = None

type command = 
  | MotionOnly of int * motion
  | SimleOperation of string

let simple_op_parser = 
  choice [
    char 'x' *> return (SimleOperation "x")
  ]

let motion_parser =
  choice [
    char 'l' *> return CharRight;
    char 'h' *> return CharLeft;
    char 'k' *> return LineUp;
    char 'j' *> return LineDown;
    char '0' *> return StartOfLine;
    char '_' *> return BeginOfLine;
    char '$' *> return EndOfLine;
    char 'w' *> return NextWordStart;
    char 'W' *> return NextFullWordStart;
    char 'e' *> return NextWordEnd;
    char 'E' *> return NextFullWordEnd;
    char 'b' *> return BackWordStart;
    char 'B' *> return BackFullWordStart;
  ] <|> return NotAMotion (* if choice fails the char is not a motion*)

let count_parser =
  option 1 (take_while1 (function '1' .. '9' -> true | _ -> false) >>| int_of_string)

let command_parser =
  simple_op_parser <|> (*if simple_of_parser fails this tries the other more complex parser for motions*)
  (count_parser >>= fun count ->
  motion_parser >>= fun motion ->
  return (MotionOnly (count, motion)))

(** parse the input string as vim normal mode command*)
let parse_command input =
  match parse_string ~consume:Consume.Prefix command_parser input with
  | Ok cmd -> cmd
  | Error msg -> failwith (msg  ^ " : unrecognized input in normal mode parser with input : " ^ input)

