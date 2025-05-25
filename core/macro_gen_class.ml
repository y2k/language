open Lib__.Common

let convert_opts opts =
  opts |> List.split_into_pairs
  |> List.map (function
       | SAtom (_, k), v -> (k, v)
       | k, v -> failsexp __LOC__ [ k; v ])

let generate_methods prefix methods =
  let methods =
    match methods with
    | SList (_, _ :: methods) -> methods
    | n -> failsexp __LOC__ [ n ]
  in
  methods
  |> List.map (function
       | SList (_, [ _; SAtom (_, name); SList (_, args); SAtom (_, ret_type) ])
         ->
           let args = List.tl args in
           let body =
             args
             |> List.mapi (fun i _ -> Printf.sprintf "p%i" i)
             |> String.concat ","
             |> Printf.sprintf "return (%s)y2k.RT.invoke(%s%s,this,%s)" ret_type
                  prefix name
           in
           let args =
             args
             |> List.mapi (fun i a ->
                    match a with
                    | SAtom (_, a) -> Printf.sprintf "%s p%i" a i
                    | x -> failsexp __LOC__ [ x ])
             |> String.concat ","
           in
           Printf.sprintf {|public %s %s(%s) {
  %s;
}|} ret_type name args body
       | x -> failsexp __LOC__ [ x ])
  |> String.concat "\n"

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
    Printf.sprintf {|public static class %s extends %s {
%s
}|} (get_value "name" "")
      (get_value "extends" "")
      (List.assoc ":methods" args |> generate_methods (get_value "prefix" "_"))
  in
  SList
    ( meta_empty,
      [ SAtom (meta_empty, "__compiler_emit"); SAtom (meta_empty, cls_code) ] )
