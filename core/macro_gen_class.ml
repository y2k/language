open Lib__.Common

let convert_opts opts =
  opts |> List.split_into_pairs
  |> List.map (function
       | SAtom (_, k), v -> (k, v)
       | k, v -> failsexp __LOC__ [ k; v ])

let generate_method args ret_type prefix name =
  let body =
    let args =
      args |> List.mapi (fun i _ -> Printf.sprintf "p%i" i) |> String.concat ","
    in
    match ret_type with
    | "void" -> Printf.sprintf "y2k.RT.invoke(%s%s,this,%s)" prefix name args
    | _ ->
        Printf.sprintf "return (%s)y2k.RT.invoke(%s%s,this,%s)" ret_type prefix
          name args
  in
  let args =
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
  let prefix = pack_string (Printf.sprintf "public %s %s(" ret_type name) in
  let suffix = pack_string (Printf.sprintf ") {\n%s;\n}" body) in
  [ prefix ] @ args @ [ suffix ]

let generate_methods prefix methods =
  let methods =
    match methods with
    | SList (_, _ :: methods) -> methods
    | n -> failsexp __LOC__ [ n ]
  in
  methods
  |> List.concat_map (function
       | SList (_, [ _; SAtom (_, name); SList (_, args); SAtom (_, ret_type) ])
         ->
           generate_method (List.tl args) ret_type prefix name
       | x -> failsexp __LOC__ [ x ])
(* |> String.concat "\n" *)

let invoke (args : sexp list) : sexp =
  let args = convert_opts args in
  let get_value name default =
    List.assoc_opt (":" ^ name) args
    |> Option.map (function
         | SAtom (_, v) -> unpack_string v
         | x -> failsexp __LOC__ [ x ])
    |> Option.value ~default
  in
  let cls_code =
    (* {|public static class %s extends %s {
%s
}|} *)
    [
      pack_string
        (Printf.sprintf "public static class %s extends " (get_value "name" ""));
      SList
        ( meta_empty,
          [
            SAtom (meta_empty, "__compiler_resolve_type");
            pack_string (get_value "extends" "");
          ] );
      pack_string " {\n";
      (* (get_value "name" "") (get_value "extends" "")); *)
    ]
    @ (List.assoc ":methods" args |> generate_methods (get_value "prefix" "_"))
    @ [ pack_string "}\n" ]
  in
  SList
    ( meta_empty,
      SAtom (meta_empty, "__compiler_emit") :: cls_code
      (* SAtom (meta_empty, "\"" ^ cls_code ^ "\""); *) )
