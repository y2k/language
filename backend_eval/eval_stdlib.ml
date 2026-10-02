open Eval_types

let list _ args = List args
let atom _ = function [ value ] -> Atom (ref value) | _ -> raise (Eval_error "atom expects one value")
let deref _ = function [ Atom cell ] -> !cell | _ -> raise (Eval_error "deref expects one atom")

let reset_BANG_ _ = function
  | [ Atom cell; value ] ->
      cell := value;
      value
  | _ -> raise (Eval_error "reset! expects an atom and a value")

let swap_BANG_ apply = function
  | [ Atom cell; (Closure _ as fn) ] ->
      let value = apply fn [ !cell ] in
      cell := value;
      value
  | _ -> raise (Eval_error "swap! expects an atom and a function")

let equal_int_float integer floating =
  (* The upper bound is exclusive: max_int rounds up to this power of two on 64-bit OCaml. *)
  let lower = float_of_int min_int in
  Float.is_finite floating && Float.is_integer floating && floating >= lower && floating < -.lower
  && int_of_float floating = integer

let rec equal_value left right =
  match (left, right) with
  | Nil, Nil -> true
  | Bool left, Bool right -> left = right
  | Int left, Int right -> left = right
  | Float left, Float right -> left = right
  | Int integer, Float floating | Float floating, Int integer -> equal_int_float integer floating
  | String left, String right -> left = right
  | Symbol left, Symbol right -> left = right
  | List left, List right -> List.equal equal_value left right
  | HashMap left, HashMap right ->
      List.equal (fun (lk, lv) (rk, rv) -> equal_value lk rk && equal_value lv rv) left right
  | _ -> false

let lookup key items =
  match List.find_opt (fun (candidate, _) -> equal_value key candidate) items with
  | Some (_, value) -> value
  | None -> Nil

let equal _ = function [] | [ _ ] -> Bool true | first :: rest -> Bool (List.for_all (equal_value first) rest)
let not _ = function [ value ] -> Bool (Stdlib.not (truthy value)) | _ -> raise (Eval_error "not expects one value")
let not_equal apply args = not apply [ equal apply args ]

let assert_ _ = function
  | [ value ] -> if truthy value then Bool true else raise (Eval_error "assertion failed")
  | _ -> raise (Eval_error "assert expects one value")

let vector_QMARK_ _ = function
  | [ List _ ] -> Bool true
  | [ _ ] -> Bool false
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
  | [ List items ] -> Int (List.length items)
  | [ HashMap items ] -> Int (List.length items)
  | _ -> raise (Eval_error "count expects one collection")

let get _ = function
  | [ Nil; _ ] -> Nil
  | [ HashMap items; key ] -> lookup key items
  | [ List _; Int index ] when index < 0 -> raise (Eval_error "get expects a nonnegative list index")
  | [ List items; Int index ] -> Option.value (List.nth_opt items index) ~default:Nil
  | [ List _; _ ] -> raise (Eval_error "get expects a numeric list index")
  | _ -> raise (Eval_error "get expects a hash-map/list and a key/index")

let get_in apply = function
  (* ponytail: vectors share List representation; no separate list-path contract. *)
  | [ collection; List keys ] -> List.fold_left (fun value key -> get apply [ value; key ]) collection keys
  | _ -> raise (Eval_error "get-in expects a collection and a vector path")

let map apply = function
  | [ (Closure _ as fn); List items ] -> List (List.map (fun item -> apply fn [ item ]) items)
  | _ -> raise (Eval_error "map expects a function and a list")

let run_BANG_ apply = function
  | [ (Closure _ as fn); List items ] ->
      List.iter (fun item -> ignore (apply fn [ item ])) items;
      Nil
  | _ -> raise (Eval_error "run! expects a function and a list")

let reduce_items = function
  | List items -> items
  | HashMap items -> List.map (fun (key, value) -> List [ key; value ]) items
  | _ -> raise (Eval_error "reduce expects a list or hash-map")

let reduce apply = function
  | [ (Closure _ as fn); collection ] -> (
      match reduce_items collection with
      | first :: rest -> List.fold_left (fun acc item -> apply fn [ acc; item ]) first rest
      | [] -> raise (Eval_error "reduce expects a non-empty collection"))
  | [ (Closure _ as fn); init; collection ] ->
      List.fold_left (fun acc item -> apply fn [ acc; item ]) init (reduce_items collection)
  | _ -> raise (Eval_error "reduce expects a function, optional initial value, and a collection")

let rec drop_items count items =
  if count <= 0 then items else match items with [] -> [] | _ :: rest -> drop_items (count - 1) rest

let drop _ = function
  | [ Int count; List items ] -> List (drop_items count items)
  | _ -> raise (Eval_error "drop expects a number and a list")

let to_int name value = match value with Int value -> value | _ -> raise (Eval_error (name ^ " expects numbers"))

let compare_numbers name fn _ = function
  | [ left; right ] -> Bool (fn (to_int name left) (to_int name right))
  | _ -> raise (Eval_error (name ^ " expects two numbers"))

let fold_numbers name init fn args = Int (List.fold_left (fun acc value -> fn acc (to_int name value)) init args)

let float_text value =
  if value >= -2147483648. && value <= 2147483647. && Float.is_integer value then string_of_int (int_of_float value)
  else
    (* ponytail: round-trip text, not cross-target exponent formatting; use a shared formatter if needed. *)
    let rec format precision =
      let text = Printf.sprintf "%.*g" precision value in
      if precision = 17 || float_of_string text = value then text else format (precision + 1)
    in
    format 1

let fold_arithmetic name int_fn float_fn first rest =
  let is_integer = function Int _ -> true | _ -> false in
  if List.for_all is_integer (first :: rest) then fold_numbers name (to_int name first) int_fn rest
  else
    let to_float = function
      | Int value -> float_of_int value
      | Float value -> value
      | _ -> raise (Eval_error (name ^ " expects numbers"))
    in
    let value = List.fold_left (fun acc value -> float_fn acc (to_float value)) (to_float first) rest in
    if value >= -2147483648. && value <= 2147483647. && Float.is_integer value then Int (int_of_float value)
    else Float value

let add _ args = fold_arithmetic "+" Stdlib.( + ) Stdlib.( +. ) (Int 0) args

let subtract _ = function
  | [] -> raise (Eval_error "- expects at least one number")
  | first :: rest -> fold_arithmetic "-" Stdlib.( - ) Stdlib.( -. ) first rest

let multiply _ args = fold_arithmetic "*" Stdlib.( * ) Stdlib.( *. ) (Int 1) args

let divide _ = function
  | [] -> raise (Eval_error "/ expects at least one number")
  | first :: rest ->
      fold_numbers "/" (to_int "/" first)
        (fun numerator divisor -> if divisor = 0 then raise (Eval_error "/ division by zero") else numerator / divisor)
        rest

let rec to_string = function
  | Nil -> "nil"
  | Bool value -> string_of_bool value
  | String name | Symbol name -> name
  | Int value -> string_of_int value
  | Float value -> float_text value
  | List items -> "(" ^ String.concat " " (List.map to_string items) ^ ")"
  | HashMap items ->
      "{" ^ String.concat " " (List.map (fun (key, value) -> to_string key ^ " " ^ to_string value) items) ^ "}"
  | Closure _ -> "#<function>"
  | Atom _ -> "#<atom>"
  | Regex _ -> "#<regex>"

let str _ args = String (String.concat "" (List.map to_string args))

let slurp _ = function
  | [ String path ] -> (
      try String (In_channel.with_open_text path In_channel.input_all)
      with Sys_error _ -> raise (Eval_error ("slurp failed: " ^ path)))
  | _ -> raise (Eval_error "slurp expects one path")

let re_pattern _ = function
  | [ String pattern ] -> (
      try Regex (Re.Perl.compile_pat pattern) with
      | Re.Perl.Parse_error -> raise (Eval_error "re-pattern: invalid pattern")
      | Re.Perl.Not_supported -> raise (Eval_error "re-pattern: unsupported pattern"))
  | _ -> raise (Eval_error "re-pattern expects one string pattern")

let re_find _ = function
  | [ Regex regex; String text ] -> (
      match Re.exec_opt regex text with Some groups -> String (Re.Group.get groups 0) | None -> Nil)
  | _ -> raise (Eval_error "re-find expects a regex and a string")

let re_replace _ = function
  | [ String text; Regex regex; String replacement ] -> String (Re.replace_string ~all:true regex ~by:replacement text)
  | _ -> raise (Eval_error "re-replace expects a string, a regex, and a string replacement")

let env =
  [
    ("re-pattern", Closure (Native re_pattern));
    ("re-find", Closure (Native re_find));
    ("re-replace", Closure (Native re_replace));
    ("list", Closure (Native list));
    ("atom", Closure (Native atom));
    ("deref", Closure (Native deref));
    ("reset!", Closure (Native reset_BANG_));
    ("swap!", Closure (Native swap_BANG_));
    ("=", Closure (Native equal));
    ("not=", Closure (Native not_equal));
    ("not", Closure (Native not));
    ("assert", Closure (Native assert_));
    (">", Closure (Native (compare_numbers ">" Stdlib.( > ))));
    ("<", Closure (Native (compare_numbers "<" Stdlib.( < ))));
    (">=", Closure (Native (compare_numbers ">=" Stdlib.( >= ))));
    ("<=", Closure (Native (compare_numbers "<=" Stdlib.( <= ))));
    ("vector?", Closure (Native vector_QMARK_));
    ("concat", Closure (Native concat));
    ("hash-map", Closure (Native hash_map));
    ("get", Closure (Native get));
    ("get-in", Closure (Native get_in));
    ("str", Closure (Native str));
    ("slurp", Closure (Native slurp));
    ("count", Closure (Native count));
    ("map", Closure (Native map));
    ("run!", Closure (Native run_BANG_));
    ("reduce", Closure (Native reduce));
    ("drop", Closure (Native drop));
    ("+", Closure (Native add));
    ("-", Closure (Native subtract));
    ("*", Closure (Native multiply));
    ("/", Closure (Native divide));
  ]
