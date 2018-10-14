(*
 * Type of types
 *)

type typ =
  | Struct of struct_entry list
  | Enum of enum_entry list

and typ_ref =
  | Slice of typ_ref
  | Array of string * typ_ref
  | Pointer of typ_ref
  | Selector of string * string list

and struct_entry =
  | StructNested of string * typ
  | StructField of string * typ_ref

and enum_entry =
  | EnumNested of string * typ
  | EnumCase of string * typ_ref option
[@@deriving sexp]

val typ_to_string : typ -> string

(*
 * AST type
 *)

type t = (string * typ) list

val t_to_string : t -> string
