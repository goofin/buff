open Std

let walk (type k) (module M: Hashtbl.Key with type t = k) (key: k) build =
  let module E = struct exception Cycle of k list end in

  let table = Hashtbl.create (module M) in
  let seen = Hash_set.create (module M) in

  let rec walk stack key =
    let stack = key :: stack in
    match (Hashtbl.find table key, Hash_set.mem seen key) with
    | (Some value, _) -> value
    | (None, true) -> raise (E.Cycle (List.rev stack))
    | (None, false) ->
      Hash_set.add seen key;
      let data = build (walk stack) key in
      Hashtbl.set table ~key ~data;
      data
  in

  try
    ignore @@ walk [] key;
    Ok table
  with | E.Cycle cycle -> Error cycle
;;
