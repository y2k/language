open Common

let convert_opts opts =
  opts |> List.split_into_pairs
  |> List.map (function
       | SAtom (_, k), v -> (k, v)
       | k, v -> failsexp __LOC__ [ k; v ])

let generate_method annot args ret_type prefix name =
  let body =
    let args =
      args |> List.mapi (fun i _ -> Printf.sprintf ",p%i" i) |> String.concat ""
    in
    match ret_type with
    | "void" -> Printf.sprintf "y2k.RT.invoke(%s%s,this%s)" prefix name args
    | _ ->
        Printf.sprintf "return (%s)y2k.RT.invoke(%s%s,this%s)" ret_type prefix
          name args
  in
  let args2 =
    args
    |> List.mapi (fun i a ->
           match a with
           | SAtom (_, a) ->
               (* Printf.sprintf "%s p%i" a i *)
               [
                 SList
                   ( meta_empty,
                     [
                       SAtom (meta_empty, "__compiler_resolve_type");
                       pack_string a;
                     ] );
                 pack_string (Printf.sprintf " p%i" i);
               ]
           | x -> failsexp __LOC__ [ x ])
    |> ( function
    | [] -> []
    | [ x ] -> [ x ]
    | x :: xs -> x :: List.concat_map (fun x -> [ [ pack_string "," ]; x ]) xs )
    |> List.flatten
  in
  (* prerr_endline @@ "LOG[annot] " ^ annot; *)
  let call_super =
    if annot = "Override" then
      let args__ =
        args
        |> List.mapi (fun i _ -> Printf.sprintf "p%i" i)
        |> String.concat ","
      in
      Printf.sprintf "super.%s(%s);\n" name args__
    else ""
  in
  let method_annot =
    match annot with
    | "Override" -> []
    | "" -> []
    | x ->
        [
          pack_string "@";
          SList
            ( meta_empty,
              [ SAtom (meta_empty, "__compiler_resolve_type"); pack_string x ]
            );
          pack_string "\n";
        ]
  in
  let prefix = pack_string (Printf.sprintf "public %s %s(" ret_type name) in
  let suffix = pack_string (Printf.sprintf ") {\n%s%s;\n}" call_super body) in
  method_annot @ [ prefix ] @ args2 @ [ suffix ]

let generate_methods prefix methods =
  let methods =
    match methods with
    | SList (_, _ :: methods) -> methods
    | n -> failsexp __LOC__ [ n ]
  in
  methods
  |> List.concat_map (function
       | SList (_, [ _; SAtom (m, name); SList (_, args); SAtom (_, ret_type) ])
         ->
           generate_method m.symbol (List.tl args) ret_type prefix name
       | x -> failsexp __LOC__ [ x ])

let generate_constructors args cls_name prefix =
  List.assoc_opt ":init" args
  |> Option.map (function
       | SAtom (_, x) ->
           let x = unpack_string x in
           [
             pack_string
               (Printf.sprintf "public %s() {\ny2k.RT.invoke(%s%s,this);\n}\n"
                  cls_name prefix x);
           ]
       | x -> failsexp __LOC__ [ x ])
  |> Option.value ~default:[]

let generate_fields args =
  List.assoc_opt ":fields" args
  |> Option.map (function
       | SList (_, SAtom (_, "vector") :: fields) ->
           fields
           |> List.map (function
                | SAtom (_, name) ->
                    pack_string
                      (Printf.sprintf "public Object %s;\n" (unpack_string name))
                | x -> failsexp __LOC__ [ x ])
       | x -> failsexp __LOC__ [ x ])
  |> Option.value ~default:[]

let invoke (args : sexp list) : sexp =
  let args = convert_opts args in
  let get_value name default =
    List.assoc_opt (":" ^ name) args
    |> Option.map (function
         | SAtom (_, v) -> unpack_string v
         | x -> failsexp __LOC__ [ x ])
    |> Option.value ~default
  in
  let prefix = get_value "prefix" "_" in
  let cls_code =
    [
      pack_string
        (Printf.sprintf "public static class %s extends " (get_value "name" ""));
      SList
        ( meta_empty,
          [
            SAtom (meta_empty, "__compiler_resolve_type");
            pack_string (get_value "extends" "Object");
          ] );
      pack_string " {\n";
    ]
    @ generate_constructors args (get_value "name" "") prefix
    @ generate_fields args
    @ (List.assoc ":methods" args |> generate_methods prefix)
    @ [ pack_string "}\n" ]
  in
  SList (meta_empty, SAtom (meta_empty, "__compiler_emit") :: cls_code)
