open Std

type type_ =
  | Struct of struct_entry list
  | Enum of enum_entry list
  | Slice of type_ref
  | Array of string * type_ref
  | Pointer of type_ref

and type_ref =
  | Literal of type_
  | Selector of string * string list

and struct_entry =
  | StructNested of string * type_
  | StructField of string * type_ref

and enum_entry =
  | EnumNested of string * type_
  | EnumCase of string * type_ref option
[@@deriving sexp]
