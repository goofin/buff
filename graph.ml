open Std

let acyclic_walk (type k) (module M: Hashtbl.Key with type t = k) key build =
  let module E = struct exception Cycle of k list end in

  let table = Hashtbl.create (module M) in
  let seen = Hash_set.create (module M) in

  let rec walk stack key =
    let stack = key :: stack in
    match (Hashtbl.find table key, Hash_set.mem seen key) with
    | (Some data, _) -> data
    | (None, true) -> raise (E.Cycle (List.rev stack))
    | (None, false) ->
      Hash_set.add seen key;
      let data = build (walk stack) key in
      Hashtbl.set table ~key ~data;
      data
  in

  try Ok (walk [] key, table)
  with | E.Cycle cycle -> Error cycle
;;

let cyclic_walk (type k) (module M: Hashtbl.Key with type t = k) key empty build =
  let table = Hashtbl.create (module M) in

  let rec walk key =
    match Hashtbl.find table key with
    | Some data -> data
    | None ->
      let data = empty key in
      Hashtbl.set table ~key ~data;
      let data = build walk data in
      Hashtbl.set table ~key ~data;
      data
  in

  (walk key, table)
;;
