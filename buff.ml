open Std

let parse lexbuf =
  try Ok (Parser.main Lexer.token lexbuf) with
  | Lexer.Error _ -> Error (Err.Lexing lexbuf)
  | Parser.Error -> Error (Err.Parsing lexbuf)

let parse_stdin () = Lexing.from_channel Stdio.stdin |> parse
let parse_str str = Lexing.from_string str |> parse

let print_type (name, type_) =
  let type_ = type_ |> Ast.sexp_of_type_ |> Sexp.to_string_hum in
  printf "%s => %s\n" name type_

let print_result = function
  | Ok types -> List.iter types ~f:print_type
  | Error err -> Err.print_error err

let () =
  parse_stdin () |> print_result

let () =
  let node, table = Graph.cyclic_walk (module String)
      "first"
      (fun key -> key)
      (fun get key -> get key)
  in
  print_endline node;
  table
  |> Hashtbl.sexp_of_t String.sexp_of_t String.sexp_of_t
  |> Sexp.to_string_hum
  |> print_endline
