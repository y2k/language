open Common

let re_find pattern str =
  let pattern =
    pattern
    |> Str.global_replace (Str.regexp "(") "\\("
    |> Str.global_replace (Str.regexp ")") "\\)"
  in
  let re = Str.regexp pattern in
  try
    let _result = Str.search_forward re str 0 in
    (* prerr_endline @@ "LOG: " ^ string_of_int _result; *)
    let groups =
      Seq.unfold
        (fun i ->
          try
            let r = Str.matched_group i str in
            Some (r, i + 1)
          with Invalid_argument _ -> None)
        1
      |> List.of_seq
    in
    Some (Str.matched_string str :: groups)
  with Not_found -> None

let attach reg_val reg_fun stdin ctx =
  ctx
  |> reg_fun "eprintln" (fun xs ->
         let s = xs |> List.map show_obj |> String.concat " " in
         prerr_endline s;
         ONil meta_empty)
  |> reg_fun "println" (fun xs ->
         let s = xs |> List.map show_obj |> String.concat " " in
         print_endline s;
         ONil meta_empty)
  |> reg_fun "boolean?" (function
       | [ OBool _ ] -> OBool (meta_empty, true)
       | _ -> OBool (meta_empty, false))
  |> reg_fun "string?" (function
       | [ OString _ ] -> OBool (meta_empty, true)
       | _ -> OBool (meta_empty, false))
  |> reg_fun "contains?" (fun xs ->
         match xs with
         | [ OMap (_, xs); k ] ->
             OBool (meta_empty, List.exists (fun (k', _) -> Obj.equal k k') xs)
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "with-meta" (fun xs ->
         match xs with
         | [ o; OString (_, symbol) ] -> (
             match o with
             | ONil m -> ONil { m with symbol }
             | OVector (m, xs) -> OVector ({ m with symbol }, xs)
             | OList (m, xs) -> OList ({ m with symbol }, xs)
             | OMap (m, xs) -> OMap ({ m with symbol }, xs)
             | OString (m, s) -> OString ({ m with symbol }, s)
             | OInt (m, i) -> OInt ({ m with symbol }, i)
             | OFloat (m, f) -> OFloat ({ m with symbol }, f)
             | OBool (m, b) -> OBool ({ m with symbol }, b)
             | OLambda (m, f) -> OLambda ({ m with symbol }, f)
             | OQuote (m, n) -> OQuote ({ m with symbol }, n))
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "boolean" (function
       | [ OBool (_, x) ] -> OBool (meta_empty, x)
       | [ ONil _ ] -> OBool (meta_empty, false)
       | [ _ ] -> OBool (meta_empty, true)
       | xs -> Obj.failobj __LOC__ xs)
  |> reg_fun "vec" (function
       | [ OList (_, xs) ] -> OVector (meta_empty, xs)
       | [ (OVector _ as x) ] -> x
       | x -> Obj.failobj __LOC__ x)
  |> reg_fun "gensym" (fun _ ->
         OQuote (meta_empty, SAtom (meta_empty, NameGenerator.get_new_var ())))
  |> reg_fun "drop" (fun xs ->
         match xs with
         | [ OInt (_, n); OList (_, xs) ] ->
             OList (meta_empty, List.filteri (fun i _ -> i >= n) xs)
         | [ OInt (_, n); OVector (_, xs) ] ->
             OVector (meta_empty, List.filteri (fun i _ -> i >= n) xs)
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "map?" (fun xs ->
         match xs with
         | [ OMap _ ] -> OBool (meta_empty, true)
         | _ -> OBool (meta_empty, false))
  |> reg_fun "=" (fun xs ->
         match xs with
         | [ x; y ] -> OBool (meta_empty, Obj.equal x y)
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "vector?" (fun xs ->
         match xs with
         | [ OVector _ ] -> OBool (meta_empty, true)
         | _ -> OBool (meta_empty, false))
  |> reg_fun "list" (fun xs -> OList (meta_empty, xs))
  |> reg_fun "string/starts-with?" (fun xs ->
         match xs with
         | [ OString (_, str); OString (_, prefix) ] ->
             OBool (meta_empty, String.starts_with ~prefix str)
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "filter" (fun xs ->
         match xs with
         | [ OLambda (_, f); OList (_, xs) ] ->
             OList
               ( meta_empty,
                 List.filter
                   (fun x ->
                     match f [ x ] with OBool (_, true) -> true | _ -> false)
                   xs )
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "hash-map" (fun xs -> OMap (meta_empty, List.split_into_pairs xs))
  |> reg_fun "string/trim" (fun xs ->
         match xs with
         | [ OString (_, x) ] -> OString (meta_empty, String.trim x)
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "re-find" (fun xs ->
         match xs with
         | [ OString (_, pattern); OString (_, str) ] -> (
             match re_find pattern str with
             | Some xs ->
                 OList
                   ( meta_empty,
                     List.map (fun x -> OString (meta_empty, x)) (List.tl xs) )
             | None -> ONil meta_empty)
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "count" (fun xs ->
         match xs with
         | [ OList (_, xs) ] -> OInt (meta_empty, List.length xs)
         | [ OVector (_, xs) ] -> OInt (meta_empty, List.length xs)
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "string/split" (fun xs ->
         match xs with
         | [ OString (_, x); OString (_, sep) ] ->
             (* FIXME: This is a hack *)
             (* let sep = Str.global_replace (Str.regexp "\\\\n") "\n" sep in *)
             (* prerr_endline @@ "|" ^ sep ^ "|"; *)
             OList
               ( meta_empty,
                 List.map
                   (fun x -> OString (meta_empty, x))
                   (String.split_on_char (String.get sep 0) x) )
         | x -> Obj.failobj __LOC__ x)
  |> reg_val "STDIN" (OString (meta_empty, stdin))
  |> reg_fun "get" (fun xs ->
         match xs with
         | [ OMap (_, m); k ] ->
             List.find_opt (fun (k', _) -> Obj.equal k k') m
             |> Option.map snd
             |> Option.value ~default:(ONil meta_empty)
         | [ OList (_, xs); OInt (_, i) ] -> List.nth xs i
         | [ OVector (_, xs); OInt (_, i) ] -> List.nth xs i
         | x -> Obj.failobj __LOC__ x)
  |> reg_val "nil" (ONil meta_empty)
  |> reg_fun "vector" (fun xs -> OVector (meta_empty, xs))
  |> reg_fun "concat" (fun xs ->
         xs
         |> List.map (function OList (_, x) -> x | x -> [ x ])
         |> List.flatten
         |> fun xs -> OList (meta_empty, xs))
  |> reg_fun "str" (fun xs ->
         xs
         |> List.map (function
              | OString (_, x) -> x
              | x -> OUtils.obj_to_string x)
         |> String.concat ""
         |> fun xs -> OString (meta_empty, xs))
  |> reg_fun "+" (function
       | [ OInt (_, x); OInt (_, y) ] -> OInt (meta_empty, x + y)
       | _ -> failwith __LOC__)
  |> reg_fun "reduce" (function
       | [ OLambda (_, f); init; OList (_, xs) ] ->
           List.fold_left (fun acc x -> f [ acc; x ]) init xs
       | [ OLambda (_, f); init; OVector (_, xs) ] ->
           List.fold_left (fun acc x -> f [ acc; x ]) init xs
       | [ OLambda (_, f); init; OMap (_, xs) ] ->
           List.fold_left
             (fun acc (k, v) -> f [ acc; OVector (meta_empty, [ k; v ]) ])
             init xs
       | [ OLambda (_, f); OList (_, xs) ] ->
           let init = List.hd xs in
           let xs = List.tl xs in
           List.fold_left (fun acc x -> f [ acc; x ]) init xs
       | x -> Obj.failobj __LOC__ x)
  |> reg_fun "map" (function
       | [ OLambda (_, f); OList (_, xs) ] ->
           OList (meta_empty, List.map (fun x -> f [ x ]) xs)
       | [ OLambda (_, f); OVector (_, xs) ] ->
           OVector (meta_empty, List.map (fun x -> f [ x ]) xs)
       | x -> Obj.failobj __LOC__ x)
