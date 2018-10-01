{
open Std
open Parser

exception Error of string * Lexing.position

let needs_semicolon = ref false
let needs_newline = ref false

let emit token =
    begin match token with
    | LEFT_BRACE
    | SEMICOLON -> needs_semicolon := false
    | _         -> needs_semicolon := true
    end;
    token

let lexing_error lexbuf =
    let invalid_input = String.make 1 (Lexing.lexeme_char lexbuf 0) in
    raise (Error (invalid_input, lexbuf.Lexing.lex_curr_p))

let string_buffer = Buffer.create 256

let add_char c =
  Buffer.add_char string_buffer c

let add_lexeme lexbuf =
  Buffer.add_string string_buffer (Lexing.lexeme lexbuf)

let grab_string () =
  let str = Buffer.contents string_buffer in
  Buffer.reset string_buffer;
  str
}

let white = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'

let digit  = ['0'-'9']
let integer = digit+
let ident  = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule token = parse
    (* comments *)
    | "//" [^ '\r' '\n' ]* { token lexbuf }
    | "/*" { read_comment lexbuf; token lexbuf }

    (* whitespace *)
    | white+  { token lexbuf }
    | newline {
        Lexing.new_line lexbuf;
        if !needs_semicolon then begin
            needs_semicolon := false;
            SEMICOLON
        end
        else token lexbuf
    }

    (* keywords *)
    | "package" { emit PACKAGE }
    | "import"  { emit IMPORT }
    | "type"    { emit TYPE }
    | "struct"  { emit STRUCT }
    | "enum"    { emit ENUM }

    (* symbols *)
    | '.'  { emit DOT }
    | ';'  { emit SEMICOLON }
    | '['  { emit LEFT_BRACKET }
    | ']'  { emit RIGHT_BRACKET }
    | '{'  { emit LEFT_BRACE }
    | '}'  { emit RIGHT_BRACE }
    | '*'  { emit STAR }

    (* literals *)
    | integer { emit (INTEGER (Lexing.lexeme lexbuf)) }
    | ident { emit (IDENT (Lexing.lexeme lexbuf)) }
    | '"' { let start = Lexing.lexeme_start_p lexbuf in
            let string = read_string lexbuf in
            lexbuf.lex_start_p <- start;
            emit (STRING string)
          }

    (* everything else *)
    | eof      { EOF }
    | _        { lexing_error lexbuf }

and read_comment = parse
    | "*/"    { }
    | newline { Lexing.new_line lexbuf; read_comment lexbuf }
    | _       { read_comment lexbuf }

and read_string = parse
    | '"'                       { grab_string () }
    | '\\' '"'                  { add_char '"';      read_string lexbuf }
    | '\\' '\\'                 { add_char '\\';     read_string lexbuf }
    | [^ '\r' '\n' '"' '\\' ]+  { add_lexeme lexbuf; read_string lexbuf }
    | _                         { lexing_error lexbuf }
    | eof                       { raise (Error ("Unclosed string literal", lexbuf.Lexing.lex_curr_p)) }
