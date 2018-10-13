open Std

type type_ =
  | Bool
  | Void
  | Integer of [`Signed | `Unsigned] * [`S8 | `S16 | `S32 | `S64]
  | Float of [`S32 | `S64]
  | Varint of [`Signed | `Unsigned | `Float ]
  | Slice of type_
  | Array of int * type_
  | Enum of enum_rec
  | Struct of struct_rec
  | Pointer of type_

and struct_rec =
  { mutable struct_fields: (string * type_) list
  ; mutable struct_types: (string * type_) list
  }

and enum_rec =
  { mutable enum_cases: (string * type_) list
  ; mutable enum_types: (string * type_) list
  }
[@@deriving sexp]

let rec size_of_type = function
  | Bool -> `Fixed 1
  | Void -> `Fixed 0
  | Integer (_, `S8) -> `Fixed 1
  | Integer (_, `S16) -> `Fixed 2
  | Integer (_, `S32) -> `Fixed 4
  | Integer (_, `S64) -> `Fixed 8
  | Float `S32 -> `Fixed 4
  | Float `S64 -> `Fixed 8
  | Varint _ -> `Variable
  | Slice _ -> `Variable
  | Array (n, type_) ->
    begin match size_of_type type_ with
      | `Fixed e -> `Fixed (n * e)
      | `Variable -> `Variable
    end
  | Enum _ -> `Variable
  | Struct _ -> `Variable
  | Pointer _ -> `Variable

let builtins = [
  ("int8", Integer (`Signed, `S8));
  ("int16", Integer (`Signed, `S16));
  ("int32", Integer (`Signed, `S32));
  ("int64", Integer (`Signed, `S64));
  ("uint8", Integer (`Unsigned, `S8));
  ("uint16", Integer (`Unsigned, `S16));
  ("uint32", Integer (`Unsigned, `S32));
  ("uint64", Integer (`Unsigned, `S64));
  ("float32", Float `S32);
  ("float64", Float `S64);
  ("int", Varint `Signed);
  ("uint", Varint `Unsigned);
  ("float", Varint `Float);
  ("bool", Bool);
  ("byte", Integer (`Unsigned, `S8)); (* or should this be distinct? *)
]

module type Scope = sig
  type t

  val root: t
  val update: t -> (string * type_) list -> t
  val find: t -> string -> type_ option
  val types: t -> (string * type_) list
end

module Scope: Scope = struct
  type t =
    { scope: (string, type_, String.comparator_witness) Map.t
    }

  let root: t =
    { scope = Map.of_alist_exn (module String) builtins
    }

  let update { scope } entries =
    let combine ~key:_ _ v2 = v2 in
    let child_scope = Map.of_alist_exn (module String) entries in
    { scope = Map.merge_skewed scope child_scope ~combine
    }

  let find { scope } = Map.find scope

  let types { scope } = Map.to_alist scope
end


let split_struct entries =
  ( List.filter_map entries ~f:(function
        | Ast.StructNested (name, type_) -> Some (name, type_)
        | _ -> None
      )
  , List.filter_map entries ~f:(function
        | Ast.StructField (name, type_ref) -> Some (name, type_ref)
        | _ -> None
      )
  )

let split_enum entries =
  ( List.filter_map entries ~f:(function
        | Ast.EnumNested (name, type_) -> Some (name, type_)
        | _ -> None
      )
  , List.filter_map entries ~f:(function
        | Ast.EnumCase (name, type_ref) -> Some (name, type_ref)
        | _ -> None
      )
  )

(* TODO(jeff): i don't understand the exception model in Base yet *)

exception Error of
    [ `DuplicateType of string
    | `UnknownType of string
    | `RecursiveType of string
    | `InvalidArray of string
    | `InvalidType of type_
    ]

let throw value = raise (Error value)

let of_ast_exn types =
  let rec empty scope entries =
    let zero scope (name, type_) =
      match type_ with
      | Ast.Struct _ -> Struct { struct_types = []; struct_fields = [] }
      | Ast.Enum _ -> Enum { enum_types = []; enum_cases = [] }
      | Ast.Slice type_ref -> Slice (resolve scope type_ref)
      | Ast.Array (n, type_ref) -> Void
      | Ast.Pointer type_ref -> Pointer (resolve scope type_ref)
    in

    match type_ with
    | Ast.Struct entries -> begin
        let self = { struct_types = []; struct_fields = [] } in
        let scope = Scope.update scope [ (name, Struct self )] in
        let types, fields = split_struct entries in
        let types = List.map types ~f:(empty scope) in
        let scope = Scope.update scope types in
        ( name, Struct self )
      end
  in

  let rec search_types types name selector =
    let (type_, types) = match List.Assoc.find ~equal:String.equal types name with
      | None -> throw @@ `UnknownType name
      | Some (Struct { struct_types; _ } as type_) -> (type_, struct_types)
      | Some (Enum { enum_types; _ } as type_) -> (type_, enum_types)
      | Some type_ -> throw @@ `InvalidType type_
    in
    match selector with
    | name :: selector -> search_types types name selector
    | [] ->  type_

  and resolve scope = function
    | Ast.Literal type_ -> build scope type_
    | Ast.Selector (name, selector) -> search_types (Scope.types scope) name selector

  and build scope = function
    | Ast.Struct entries -> begin
        let types, fields = split_struct entries in
        let types = build_all scope types in
        let scope = Scope.update scope types in
        let fields = List.map fields ~f:(fun (name, type_ref) ->
            (n    e,  es  ve  co   ne   pe_ref)
          ) in
        Struct { types; fields }
      end
    | Ast.Enum entries -> begin
        let types, cases = split_enum entries in
        let types = build_all scope types in
        let scope = Scope.update scope name types in
        let cases = List.map cases ~f:(fun (name, type_ref) ->
            ( name, match type_ref with
                | None -> Void
                | Some type_ref -> resolve scope name type_ref
            )
          ) in
        Enum { types; cases }
      end
    | Ast.Slice type_ref -> Slice (resolve scope name type_ref)
    | Ast.Array (n, type_ref) -> Void
    | Ast.Pointer type_ref -> Pointer (resolve scope name type_ref)

  and build_all scope types =


  in
  build_all Scope.root types

let of_ast types =
  try Ok (of_ast_exn types) with
  | Error err -> Error err

