open Std

let parse lexbuf =
  try Ok (Parser.main Lexer.token lexbuf) with
  | Lexer.Error _ -> Error (Err.Lexing lexbuf)
  | Parser.Error -> Error (Err.Parsing lexbuf)

let parse_stdin () = Lexing.from_channel Stdio.stdin |> parse
let parse_str str = Lexing.from_string str |> parse

let print = function
  | Ok statements -> List.iter statements ~f:(
      Ast.sexp_of_statement
      >> Sexp.to_string_hum
      >> print_endline
    )
  | Error err -> Err.print_error err

let () =
  begin match Graph.walk (module String) "first" (fun get key -> get key) with
    | Ok _ -> print_endline "Didn't detect cycle"
    | Error cycle -> print_endline @@ String.concat ~sep:" -> " cycle
  end;
  parse_stdin () |> print
