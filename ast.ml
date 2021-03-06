open Std

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

let typ_to_string =
  sexp_of_typ >> Sexp.to_string_hum

(*
 * AST type
 *)

type t = (string * typ) list
[@@deriving sexp]

let t_to_string =
  sexp_of_t >> Sexp.to_string_hum

