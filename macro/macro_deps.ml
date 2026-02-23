open Core__.Common

let invoke _ctx _simplify = function
  | SList (_, [ SAtom (_, "deps"); SList (_, SAtom (_, "hash-map") :: pairs) ])
    ->
      let deps =
        pairs |> List.split_into_pairs
        |> List.map (fun (k, v) ->
            let key =
              match k with
              | SAtom (_, s) when String.starts_with ~prefix:":" s ->
                  String.sub s 1 (String.length s - 1)
              | _ -> failsexp __LOC__ [ k ]
            in
            let version =
              match v with
              | SAtom (_, s) when String.starts_with ~prefix:"\"" s ->
                  unpack_string s
              | _ -> failsexp __LOC__ [ v ]
            in
            SList
              ( meta_empty,
                [ SAtom (meta_empty, key); SAtom (meta_empty, version) ] ))
      in
      Some
        (SList
           ( meta_empty,
             [
               SAtom (meta_empty, "def*");
               SAtom (meta_empty, "__deps");
               SList
                 ( meta_empty,
                   [ SAtom (meta_empty, "quote*"); SList (meta_empty, deps) ] );
             ] ))
  | _ -> None
