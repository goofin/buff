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

    (* everything else *)
    | eof      { EOF }
    | _        { lexing_error lexbuf }

and read_comment = parse
    | "*/"    { }
    | newline { Lexing.new_line lexbuf; read_comment lexbuf }
    | _       { read_comment lexbuf }
