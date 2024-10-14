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

type operation = None

type command =
  | Partial of int 
  | NotACommand
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
  ] 

let count_parser =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let command_parser =
  simple_op_parser (*if simple_of_parser fails this tries the other more complex parser for motions*)
  <|> (
  motion_parser >>= fun motion ->
  return (MotionOnly (1, motion))
  )
  <|> (count_parser >>= fun count ->
  motion_parser >>= fun motion ->
  return (MotionOnly (count, motion))) 
  <|> (count_parser >>= fun count -> return (Partial (count))) 

(** parse the input string as vim normal mode command*)
let parse_command input =
  match parse_string ~consume:Consume.All command_parser input with
  | Ok cmd -> cmd
  | Error _ -> NotACommand

