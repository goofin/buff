open Std

type statement =
  | Package of string
  | Import of string option * string
  | Type of string * type_literal

and type_ref =
  | Literal of type_literal
  | Selector of string * type_ref option

and type_literal =
  | Struct of struct_entry list
  | Enum of enum_entry list
  | Slice of type_ref
  | Array of string * type_ref
  | Pointer of type_ref

and struct_entry =
  | StructNested of string * type_literal
  | StructField of string * type_ref

and enum_entry =
  | EnumNested of string * type_literal
  | EnumCase of string * type_ref option

[@@deriving sexp]
