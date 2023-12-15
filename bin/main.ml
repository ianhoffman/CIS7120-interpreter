open Interpreter
open Result

let run_interpreter inp =
  let ret = Parser.parse inp in
  match ret with
  | Ok p -> Evaluator.interpret p
  | Error e -> print_endline ("ERROR: " ^ e)

let () =
  let args = Sys.argv in
  if Array.length args != 2 then
    print_endline
      ("expected a single string, wrapped in quotes, as an argument. Got "
      ^ string_of_int (Array.length args)
      ^ " args")
  else run_interpreter args.(1)
