open Std

type error =
  | Lexing of Lexing.lexbuf
  | Parsing of Lexing.lexbuf

let positions (lexbuf: Lexing.lexbuf) =
  let start = lexbuf.lex_start_p in
  let curr = lexbuf.lex_curr_p in
  start.pos_lnum,
  start.pos_cnum - start.pos_bol,
  curr.pos_cnum - curr.pos_bol

let error_to_string error =
  let buf = Buffer.create 100 in
  let append = Buffer.add_string buf in
  let sprintf = Printf.sprintf in

  let lexbuf, kind = match error with
    | Lexing lexbuf -> lexbuf, "syntax"
    | Parsing lexbuf -> lexbuf, "parse"
  in

  let line, s_col, c_col = positions lexbuf in
  let carrots = c_col - s_col in
  if carrots = 0 then
    append @@ sprintf "%s error: unexpected EOF\n" kind

  else begin
    append @@ sprintf "%s error:\n\n" kind;

    let lines = lexbuf.lex_buffer |> Bytes.to_string |> String.split_lines in
    for n = line - 3 to line - 1 do
      match List.nth lines n with
      | Some line -> append @@ sprintf "%4d: %s\n" (n + 1) line
      | None -> ()
    done;

    append @@ sprintf "%s%s%s\n"
      (String.make 6 ' ')
      (String.make s_col '~')
      (String.make carrots '^')
  end;

  Buffer.contents buf

let lexbuf lexbuf =
  try Ok (Parser.main Lexer.token lexbuf) with
  | Lexer.Error _ -> Error (Lexing lexbuf)
  | Parser.Error -> Error (Parsing lexbuf)

let stdin () =
  Lexing.from_channel Stdio.stdin |> lexbuf

let string string =
  Lexing.from_string string |> lexbuf
