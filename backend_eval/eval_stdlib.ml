open Eval_types

let list _ args = List args

let rec equal_value left right =
  match (left, right) with
  | Symbol left, Symbol right -> left = right
  | List left, List right -> List.equal equal_value left right
  | HashMap left, HashMap right ->
      List.equal (fun (lk, lv) (rk, rv) -> equal_value lk rk && equal_value lv rv) left right
  | _ -> false

let equal _ = function
  | [] | [ _ ] -> Symbol "true"
  | first :: rest -> Symbol (if List.for_all (equal_value first) rest then "true" else "false")

let vector_QMARK_ _ = function
  | [ List _ ] -> Symbol "true"
  | [ _ ] -> Symbol "false"
  | _ -> raise (Eval_error "vector? expects one value")

let concat _ args =
  args |> List.concat_map (function List items -> items | _ -> raise (Eval_error "concat expects lists"))
  |> fun items -> List items

let hash_map _ args =
  let rec loop items acc =
    match items with
    | [] -> HashMap (Stdlib.List.rev acc)
    | key :: value :: rest -> loop rest ((key, value) :: acc)
    | _ -> raise (Eval_error "hash-map arguments must be key/value pairs")
  in
  loop args []

let count _ = function
  | [ List items ] -> Symbol (string_of_int (List.length items))
  | [ HashMap items ] -> Symbol (string_of_int (List.length items))
  | _ -> raise (Eval_error "count expects one collection")

let get _ = function
  | [ HashMap items; key ] -> Option.value (List.assoc_opt key items) ~default:(Symbol "nil")
  | [ List items; Symbol index ] -> (
      match int_of_string_opt index with
      | Some index -> Option.value (List.nth_opt items index) ~default:(Symbol "nil")
      | None -> raise (Eval_error "get expects a numeric list index"))
  | _ -> raise (Eval_error "get expects a hash-map/list and a key/index")

let map apply = function
  | [ fn; List items ] -> List (List.map (fun item -> apply fn [ item ]) items)
  | _ -> raise (Eval_error "map expects a function and a list")

let reduce_items = function
  | List items -> items
  | HashMap items -> List.map (fun (key, value) -> List [ key; value ]) items
  | _ -> raise (Eval_error "reduce expects a list or hash-map")

let reduce apply = function
  | [ fn; collection ] -> (
      match reduce_items collection with
      | first :: rest -> List.fold_left (fun acc item -> apply fn [ acc; item ]) first rest
      | [] -> raise (Eval_error "reduce expects a non-empty collection"))
  | [ fn; init; collection ] -> List.fold_left (fun acc item -> apply fn [ acc; item ]) init (reduce_items collection)
  | _ -> raise (Eval_error "reduce expects a function, optional initial value, and a collection")

let rec drop_items count items =
  if count <= 0 then items else match items with [] -> [] | _ :: rest -> drop_items (count - 1) rest

let drop _ = function
  | [ Symbol count; List items ] -> (
      match int_of_string_opt count with
      | Some count -> List (drop_items count items)
      | None -> raise (Eval_error "drop expects a number and a list"))
  | _ -> raise (Eval_error "drop expects a number and a list")

let to_int name value =
  match value with
  | Symbol value -> (
      match int_of_string_opt value with Some value -> value | None -> raise (Eval_error (name ^ " expects numbers")))
  | _ -> raise (Eval_error (name ^ " expects numbers"))

let fold_numbers name init fn args =
  args |> List.fold_left (fun acc value -> fn acc (to_int name value)) init |> string_of_int |> fun value ->
  Symbol value

let add _ args = fold_numbers "+" 0 Stdlib.( + ) args

let subtract _ = function
  | [] -> raise (Eval_error "- expects at least one number")
  | first :: rest -> fold_numbers "-" (to_int "-" first) Stdlib.( - ) rest

let multiply _ args = fold_numbers "*" 1 Stdlib.( * ) args

let divide _ = function
  | [] -> raise (Eval_error "/ expects at least one number")
  | first :: rest -> fold_numbers "/" (to_int "/" first) Stdlib.( / ) rest

let rec to_string = function
  | Symbol name -> name
  | List items -> "(" ^ String.concat " " (List.map to_string items) ^ ")"
  | HashMap items ->
      "{" ^ String.concat " " (List.map (fun (key, value) -> to_string key ^ " " ^ to_string value) items) ^ "}"
  | Closure _ -> "#<function>"

let str _ args = Symbol (String.concat "" (List.map to_string args))

let env =
  [
    ("list", Closure (Native list));
    ("=", Closure (Native equal));
    ("vector?", Closure (Native vector_QMARK_));
    ("concat", Closure (Native concat));
    ("hash-map", Closure (Native hash_map));
    ("get", Closure (Native get));
    ("str", Closure (Native str));
    ("count", Closure (Native count));
    ("map", Closure (Native map));
    ("reduce", Closure (Native reduce));
    ("drop", Closure (Native drop));
    ("+", Closure (Native add));
    ("-", Closure (Native subtract));
    ("*", Closure (Native multiply));
    ("/", Closure (Native divide));
  ]
