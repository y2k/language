open Core__.Common
open Printf

(* gen-class macro for java_v2 backend - uses direct static method calls *)

let parse_opts opts =
  opts |> List.split_into_pairs
  |> List.map (function
    | SAtom (_, k), v -> (k, v)
    | k, v -> failsexp __LOC__ [ k; v ])

let get_opt_value name default opts =
  List.assoc_opt (":" ^ name) opts
  |> Option.map (function
    | SAtom (_, v) -> unpack_string v
    | x -> failsexp __LOC__ [ x ])
  |> Option.value ~default

let mk_type_resolve typ =
  SList
    ( meta_empty,
      [ SAtom (meta_empty, "__compiler_resolve_type"); pack_string typ ] )

let generate_method_annotation = function
  | "Override" -> ([], false)
  | ":static" -> ([], true)
  | "" -> ([], false)
  | annot ->
      ([ pack_string "@"; mk_type_resolve annot; pack_string "\n" ], false)

let generate_method_args args =
  args
  |> List.mapi (fun i a ->
      match a with
      | SAtom (_, typ) ->
          [
            mk_type_resolve (unpack_string typ); pack_string (sprintf " p%i" i);
          ]
      | x -> failsexp __LOC__ [ x ])
  |> ( function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: xs -> x :: List.concat_map (fun x -> [ [ pack_string "," ]; x ]) xs )
  |> List.flatten

let generate_method_body is_static ret_type prefix name args =
  let args_str =
    args |> List.mapi (fun i _ -> sprintf "p%i" i) |> String.concat ","
  in
  (* For java_v2: use direct static method call instead of RT.invoke *)
  let invoke_call =
    if is_static then sprintf "%s%s(%s)" prefix name args_str
    else
      let args_with_this =
        if args_str = "" then "this" else "this," ^ args_str
      in
      sprintf "%s%s(%s)" prefix name args_with_this
  in
  match ret_type with
  | "void" -> invoke_call
  | _ -> sprintf "return (%s)%s" ret_type invoke_call

let generate_method annot args ret_type prefix name =
  let method_annot, is_static = generate_method_annotation annot in
  let body = generate_method_body is_static ret_type prefix name args in
  let args_code = generate_method_args args in
  let call_super =
    if annot = "Override" then
      let args_str =
        args |> List.mapi (fun i _ -> sprintf "p%i" i) |> String.concat ","
      in
      sprintf "super.%s(%s);\n" name args_str
    else ""
  in
  let static_mod = if is_static then "static " else "" in
  let throws = " throws Exception" in
  method_annot
  @ [ pack_string (sprintf "public %s%s %s(" static_mod ret_type name) ]
  @ args_code
  @ [ pack_string (sprintf ")%s {\n%s%s;\n}" throws call_super body) ]

let generate_methods prefix methods =
  let methods =
    match methods with
    | SList (_, _ :: methods) -> methods
    | n -> failsexp __LOC__ [ n ]
  in
  methods
  |> List.concat_map (function
    | SList (_, [ _; SAtom (m, name); SList (_, args); SAtom (_, ret_type) ]) ->
        generate_method m.symbol (List.tl args) ret_type prefix name
    | x -> failsexp __LOC__ [ x ])

let generate_constructors cls_name prefix opts =
  List.assoc_opt ":init" opts
  |> Option.map (function
    | SAtom (_, init_fn) ->
        let fn = unpack_string init_fn in
        (* For java_v2: use direct static method call *)
        [
          pack_string
            (sprintf "public %s() throws Exception {\n%s%s(this);\n}\n" cls_name
               prefix fn);
        ]
    | x -> failsexp __LOC__ [ x ])
  |> Option.value ~default:[]

let generate_fields opts =
  List.assoc_opt ":fields" opts
  |> Option.map (function
    | SList (_, SAtom (_, "vector") :: fields) ->
        fields
        |> List.map (function
          | SAtom (_, name) ->
              pack_string (sprintf "public Object %s;\n" (unpack_string name))
          | x -> failsexp __LOC__ [ x ])
    | x -> failsexp __LOC__ [ x ])
  |> Option.value ~default:[]

let invoke = function
  | SList (_, SAtom (_, "gen-class") :: args) ->
      let opts = parse_opts args in
      let prefix = get_opt_value "prefix" "_" opts in
      let cls_name = get_opt_value "name" "" opts in
      let extends = get_opt_value "extends" "Object" opts in
      let cls_code =
        [ pack_string (sprintf "public static class %s extends " cls_name) ]
        @ [ mk_type_resolve extends; pack_string " {\n" ]
        @ generate_constructors cls_name prefix opts
        @ generate_fields opts
        @ generate_methods prefix (List.assoc ":methods" opts)
        @ [ pack_string "}\n" ]
      in
      SList (meta_empty, SAtom (meta_empty, "__compiler_emit") :: cls_code)
      |> Option.some
  | _ -> None
