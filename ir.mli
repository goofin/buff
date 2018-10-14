(*
 * Type of types
 *)

type typ =
  | Bool
  | Void
  | Integer of [`Signed | `Unsigned] * [`S8 | `S16 | `S32 | `S64]
  | Float of [`S32 | `S64]
  | Varint of [`Signed | `Unsigned | `Float ]
  | Struct of struct_rec
  | Enum of enum_rec
  | Slice of typ
  | Pointer of typ
  | Array of int * typ

and struct_rec =
  { struct_name : string list
  ; mutable struct_fields: (string * typ) list
  ; mutable struct_types: (string * typ) list
  }

and enum_rec =
  { enum_name : string list
  ; mutable enum_cases: (string * typ) list
  ; mutable enum_types: (string * typ) list
  }
[@@deriving sexp]

val typ_to_string : typ -> string
val typ_to_size : typ -> [`Fixed of int | `Variable]

(*
 * Error type
 *)

type error

val error_to_string : error -> string

(*
 * IR type
 *)

type t = (string * typ) list

val t_to_string : t -> string
val ast_to_t : Ast.t -> (t, error) result
val ast_to_t_exn : Ast.t -> t
