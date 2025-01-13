open Common

let generate_class (compile_exp : sexp -> string) prefix params clsName (methods : sexp list) superCls =
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
    |> List.reduce __LOC__ (Printf.sprintf "%s%s")
  in
  Printf.sprintf "public static class %s extends %s {\npublic java.util.List<Object> state;\n%s%s}" clsName superCls
    state ms

let invoke compile context = function
  | SList
      ( _,
        [
          SAtom (_, "gen-class-inner");
          SList
            ( _,
              [
                _;
                SList
                  ( _,
                    [
                      SAtom (_, ":name");
                      SAtom (_, clsName);
                      SAtom (_, ":extends");
                      SAtom (_, superCls);
                      SAtom (_, ":constructors");
                      SList (_, [ SList (_, params); SList _ ]);
                      SAtom (_, ":prefix");
                      SAtom (_, prefix);
                      SAtom (_, ":methods");
                      SList (_, methods);
                    ] );
              ] );
        ] ) ->
      (context, generate_class compile prefix params clsName methods superCls)
  | n -> failsexp __LOC__ [ n ]
