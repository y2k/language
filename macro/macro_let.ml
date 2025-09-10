open Core__.Common

let rec loop = function
  | [] -> []
  | (SAtom (_, "_"), _) :: tail -> loop tail
  | ((SAtom _, _) as kv) :: tail -> kv :: loop tail
  | ((SList (_, SAtom (_, "vector") :: _) as k), (SList _ as v)) :: tail ->
      let nv = SAtom (meta_empty, NameGenerator.get_new_var ()) in
      loop ((nv, v) :: (k, nv) :: tail)
  | (SList (_, SAtom (_, "vector") :: kps), (SAtom _ as v)) :: tail ->
      let xs =
        kps
        |> List.mapi (fun i k ->
               ( k,
                 SList
                   ( meta_empty,
                     [
                       SAtom (meta_empty, "get");
                       v;
                       SAtom (meta_empty, string_of_int i);
                     ] ) ))
      in
      loop (xs @ tail)
  | xs -> failsexp __LOC__ (List.concat_map (fun (k, v) -> [ k; v ]) xs)

let mk_let k v = SList (meta_empty, [ SAtom (meta_empty, "let*"); k; v ])

let invoke simplify = function
  | SList
      (m2, SAtom (_, "let") :: SList (_, SAtom (_, "vector") :: vals) :: body)
    ->
      let lets =
        loop (List.split_into_pairs vals)
        |> List.map (fun (k, v) -> mk_let k (simplify v))
      in
      SList (m2, (SAtom (meta_empty, "do") :: lets) @ body)
      |> simplify |> Option.some
  | SList (m, SAtom (ml, "let") :: name :: value) ->
      let value = List.map simplify value in
      SList (m, SAtom (ml, "let*") :: name :: value) |> Option.some
  | _ -> None
