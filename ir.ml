open Std

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
  { struct_name: string list
  ; mutable struct_fields: (string * typ) list
  ; mutable struct_types: (string * typ) list
  }

and enum_rec =
  { enum_name: string list
  ; mutable enum_cases: (string * typ) list
  ; mutable enum_types: (string * typ) list
  }
[@@deriving sexp]

let typ_to_string = sexp_of_typ >> Sexp.to_string_hum

let rec typ_to_size = function
  | Bool -> `Fixed 1
  | Void -> `Fixed 0
  | Integer (_, `S8) -> `Fixed 1
  | Integer (_, `S16) -> `Fixed 2
  | Integer (_, `S32) -> `Fixed 4
  | Integer (_, `S64) -> `Fixed 8
  | Float `S32 -> `Fixed 4
  | Float `S64 -> `Fixed 8
  | Varint _ -> `Variable
  | Struct _ -> `Variable
  | Enum _ -> `Variable
  | Slice _ -> `Variable
  | Pointer _ -> `Variable
  | Array (n, typ) -> begin
      match typ_to_size typ with
      | `Fixed e -> `Fixed (n * e)
      | `Variable -> `Variable
    end

(*
 * Error type
 *)

type error =
  [ `DuplicateType of string
  | `UnknownType of string
  | `RecursiveType of string
  | `InvalidArray of string
  | `InvalidType of typ
  ]
[@@deriving sexp]

let error_to_string =
  sexp_of_error >> Sexp.to_string_hum

exception Error of error
let throw value = raise (Error value)

(*
 * IR Type
 *)

type t = (string * typ) list
[@@deriving sexp]

let builtins =
  let build name typ = (name, typ) in
  [ build "int8" (Integer (`Signed, `S8))
  ; build "int16" (Integer (`Signed, `S16))
  ; build "int32" (Integer (`Signed, `S32))
  ; build "int64" (Integer (`Signed, `S64))
  ; build "uint8" (Integer (`Unsigned, `S8))
  ; build "uint16" (Integer (`Unsigned, `S16))
  ; build "uint32" (Integer (`Unsigned, `S32))
  ; build "uint64" (Integer (`Unsigned, `S64))
  ; build "float32" (Float `S32)
  ; build "float64" (Float `S64)
  ; build "int" (Varint `Signed)
  ; build "uint" (Varint `Unsigned)
  ; build "float" (Varint `Float)
  ; build "bool" (Bool)
  ; build "byte" (Integer (`Unsigned, `S8)) (* or should this be distinct? *)
  ]

module type Scope = sig
  type t

  val create: (string * typ) list -> t
  val update: t -> string -> (string * typ) list -> t
  val find: t -> string -> typ option
  val types: t -> (string * typ) list
end

module Scope: Scope = struct
  type t =
    { scope: (string, typ, String.comparator_witness) Map.t
    ; stack: string list
    }

  let root = Map.of_alist_exn (module String) builtins

  let combine ~key:_ _ v2 = v2

  let create entries =
    let child = Map.of_alist_exn (module String) entries in
    { scope = Map.merge_skewed root child ~combine
    ; stack = []
    }

  let update { scope; stack } name entries =
    let child = Map.of_alist_exn (module String) entries in
    { scope = Map.merge_skewed scope child ~combine
    ; stack = name :: stack
    }

  let find { scope; _ } = Map.find scope

  let types { scope; _ } = Map.to_alist scope
end

let struct_types entries =
  List.filter_map entries ~f:(function
      | Ast.StructNested (name, typ) -> Some (name, typ)
      | _ -> None
    )

let struct_fields entries =
  List.filter_map entries ~f:(function
      | Ast.StructField (name, typ_ref) -> Some (name, Some typ_ref)
      | _ -> None
    )

let enum_types entries =
  List.filter_map entries ~f:(function
      | Ast.EnumNested (name, typ) -> Some (name, typ)
      | _ -> None
    )

let enum_cases entries =
  List.filter_map entries ~f:(function
      | Ast.EnumCase (name, typ_ref) -> Some (name, typ_ref)
      | _ -> None
    )

let ast_to_t_exn types =
  let rec empty stack types =
    List.filter_map types ~f:(fun (name, typ) ->
        let full_name = name :: stack in
        match typ with
        | Ast.Struct entries ->
          let types = entries |> struct_types |> empty full_name in
          Some (name, Struct
                  { struct_name = full_name
                  ; struct_types = types
                  ; struct_fields = []
                  } )
        | Ast.Enum entries ->
          let types = entries |> enum_types |> empty full_name in
          Some (name, Enum
                  { enum_name = full_name
                  ; enum_types = types
                  ; enum_cases = []
                  } )
      )
  in

  let rec search_types types name selector =
    let (typ, types) = match List.Assoc.find ~equal:String.equal types name with
      | None -> throw @@ `UnknownType name
      | Some (Struct { struct_types; _ } as typ) -> (typ, struct_types)
      | Some (Enum { enum_types; _ } as typ) -> (typ, enum_types)
      | Some typ -> (typ, [])
    in
    match selector with
    | name :: selector -> search_types types name selector
    | [] ->  typ
  in

  let rec resolve scope name typ_ref =
    match typ_ref with
    | Ast.Selector (name, selector) -> search_types (Scope.types scope) name selector
    | Ast.Slice typ_ref -> Slice (resolve scope name typ_ref)
    | Ast.Pointer typ_ref -> Pointer (resolve scope name typ_ref)
    | Ast.Array (n, typ_ref) ->
      let n = try Int.of_string n with | _ -> throw @@ `InvalidArray n in
      let typ = resolve scope name typ_ref in
      Array (n, typ)
  in

  let rec build scope name typ =
    let (types, entries) = match typ with
      | Ast.Struct entries -> ( struct_types entries, struct_fields entries )
      | Ast.Enum entries -> ( enum_types entries, enum_cases entries )
    in

    (* if we're building a struct/enum in scope, add all of the subtypes to the scope *)
    let scope = match Scope.find scope name with
      | Some (Struct { struct_types; _ }) -> Scope.update scope name struct_types
      | Some (Enum { enum_types; _ }) -> Scope.update scope name enum_types
      | _ -> scope
    in

    (* since all the types are necessarily in scope, build them *)
    let types = build_all scope types in

    (* now we can build the struct/enum entries, if any *)
    let entries = List.map entries ~f:(fun (name, typ_ref) ->
        match typ_ref with
        | None -> (name, Void)
        | Some typ_ref -> (name, resolve scope name typ_ref)
      ) in

    ( name
    , match Scope.find scope name with
    | Some (Struct self) ->
      self.struct_types <- types;
      self.struct_fields <- entries;
      Struct self

    | Some (Enum self) ->
      self.enum_types <- types;
      self.enum_cases <- entries;
      Enum self

    | Some typ -> throw @@ `InvalidType typ
    | None -> throw @@ `UnknownType name
    )

  and build_all scope types =
    List.map types ~f:(fun (name, typ) -> build scope name typ)

  in
  let scope = Scope.create (empty [] types) in
  build_all scope types

let ast_to_t types =
  try Ok (ast_to_t_exn types) with
  | Error err -> Error err

let t_to_string =
  let module ToString = struct
    type ir_typ = typ
    [@@deriving sexp]

    type typ =
      [ `Basic of ir_typ
      | `Named of string list
      | `Struct of (string * typ) list
      | `Enum of (string * typ) list
      | `Slice of typ
      | `Pointer of typ
      | `Array of int * typ
      ]
    [@@deriving sexp]

    type t = (string * typ) list
    [@@deriving sexp]

    let rec flatten (name, typ) =
      let types = match typ with
        | Struct { struct_types; _ } -> struct_types
        | Enum { enum_types; _ } -> enum_types
        | _ -> []
      in
      (name, typ) :: List.concat_map types ~f:flatten

    let rec transform (name, typ) =
      ( name
      , match typ with
      | Slice typ -> `Slice (transform_entry typ)
      | Pointer typ -> `Pointer (transform_entry typ)
      | Array (n, typ) -> `Array (n, transform_entry typ)
      | Struct { struct_fields; _ } ->
        let names, types = List.unzip struct_fields in
        let types = List.map types ~f:transform_entry in
        `Struct (List.zip_exn names types)
      | Enum { enum_cases; _ } ->
        let names, types = List.unzip enum_cases in
        let types = List.map types ~f:transform_entry in
        `Enum (List.zip_exn names types)
      | typ -> `Basic typ
      )

    and transform_entry typ =
      match typ with
      | Slice typ -> `Slice (transform_entry typ)
      | Pointer typ -> `Pointer (transform_entry typ)
      | Array (n, typ) -> `Array (n, transform_entry typ)
      | Struct { struct_name; _ } -> `Named struct_name
      | Enum { enum_name; _ } -> `Named enum_name
      | typ -> `Basic typ

    let ir_to_t t: t =
      List.concat_map t ~f:flatten
      |> List.map ~f:transform
  end in

  ToString.ir_to_t
  >> ToString.sexp_of_t
  >> Sexp.to_string_hum
