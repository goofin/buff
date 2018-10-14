open Std

let () =
  match Parse.stdin () with
  | Error err -> print_endline @@ Parse.error_to_string err
  | Ok ast  ->
    ast |> Ast.t_to_string |> print_endline;
    Ir.ast_to_t_exn ast |> Ir.t_to_string |> print_endline
