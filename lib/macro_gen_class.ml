open Common

let generate_class (compile_exp : sexp -> string) prefix params clsName (methods : sexp list) superCls annotations =
  let prefix = String.sub prefix 1 (String.length prefix - 2) in
  let cnt_params =
    match params with
    | [] -> ""
    | params ->
        params
        |> List.mapi (fun i x ->
               let type1 = compile_exp x in
               let type2 =
                 if String.starts_with ~prefix:"\"" type1 then String.sub type1 1 (String.length type1 - 2) else type1
               in
               Printf.sprintf "%s p%i" type2 i)
        |> List.reduce __LOC__ (Printf.sprintf "%s,%s")
  in
  let state =
    match params with
    | [] -> ""
    | params ->
        params
        |> List.mapi (fun i _ -> Printf.sprintf "p%i" i)
        |> List.reduce __LOC__ (Printf.sprintf "%s,%s")
        |> Printf.sprintf "public %s(%s) {\nstate=java.util.List.of(%s);\n}\n" clsName cnt_params
  in
  let ms =
    methods
    |> List.map (function
         | SList (_, [ SAtom (m, mname); SList (_, args); SAtom (_, rtype) ]) ->
             let args_ =
               args
               |> List.mapi (fun i a ->
                      match a with
                      | SAtom (_, a) when String.starts_with ~prefix:"\"" a ->
                          Printf.sprintf "%s p%i" (unpack_string a) i
                      | SAtom (_, a) -> Printf.sprintf "%s p%i" a i
                      | x -> failsexp __LOC__ [ x ])
               |> List.reduce_opt (Printf.sprintf "%s, %s")
               |> Option.value ~default:""
             in
             let args__ =
               args
               |> List.mapi (fun i _ -> Printf.sprintf "p%i" i)
               |> List.reduce_opt (Printf.sprintf "%s, %s")
               |> Option.value ~default:""
             in
             let annot = match m.symbol with "" -> "" | x -> "@" ^ x ^ " " in
             let return_ = if rtype = "void" then "" else Printf.sprintf "return (%s)" rtype in
             let call_super = if annot = "@Override " then Printf.sprintf "super.%s(%s);\n" mname args__ else "" in
             let full_args = match args__ with "" -> "this" | x -> "this, " ^ x in
             Printf.sprintf "%spublic %s %s(%s) {\n%s%s%s%s(%s); }\n" annot rtype mname args_ call_super return_ prefix
               mname full_args
         | x -> failsexp __LOC__ [ x ])
    |> List.fold_left (Printf.sprintf "%s%s") ""
  in
  let annotations =
    if annotations = [] then ""
    else
      annotations
      |> List.fold_left
           (fun acc x ->
             let x = unpack_string x in
             Printf.sprintf "%s@%s\n" acc x)
           ""
  in
  Printf.sprintf "%spublic static class %s extends %s {\npublic java.util.List<Object> state;\n%s%s}" annotations
    clsName superCls state ms

let invoke compile context = function
  | SList (_, [ SAtom (_, "gen-class-inner"); SList (_, [ _; SList (_, props) ]) ]) ->
      let props = props |> List.split_into_pairs in
      let get_string_value name def =
        props
        |> List.find_map (function SAtom (_, n), SAtom (_, p) when n = name -> Some p | _ -> None)
        |> Option.value ~default:def
      in

      let prefix = get_string_value ":prefix" "\"_\"" in
      let clsName = get_string_value ":name" "App" in
      let superCls = get_string_value ":extends" "Object" in
      let methods =
        props
        |> List.find_map (function SAtom (_, n), SList (_, xs) when n = ":methods" -> Some xs | _ -> None)
        |> Option.value ~default:[]
      in

      let params =
        props
        |> List.find_map (function SAtom (_, n), SList (_, xs) when n = ":constructors" -> Some xs | _ -> None)
        |> Option.value ~default:[]
        |> List.find_map (function SList (_, xs) -> Some xs | _ -> None)
        |> Option.value ~default:[]
      in

      let annotations =
        props
        |> List.find_map (function SAtom (_, n), SList (_, xs) when n = ":annotations" -> Some xs | _ -> None)
        |> Option.value ~default:[]
        |> List.map (function SAtom (_, x) -> x | x -> failsexp __LOC__ [ x ])
      in

      (context, generate_class compile prefix params clsName methods superCls annotations)
  | n -> failsexp __LOC__ [ n ]
