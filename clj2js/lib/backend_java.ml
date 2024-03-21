module A = Angstrom
open Frontend

let rec compile_ (context : context) (node : cljexp) : context * string =
  let compile node = compile_ context node |> snd in
  let compile2 node =
    let ctx2, _ = compile_ context node in
    ctx2.out_var
  in
  let with_context node = (context, node) in
  let with_context2 out_var node = ({ context with out_var }, node) in
  match node with
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      let r = "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" in
      ({ context with out_var = r }, "")
  | Atom (_, x) -> ({ context with out_var = x }, "")
  | RBList [ Atom (_, "+"); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s+%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, "-"); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s-%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, "*"); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s*%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, "/"); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s/%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, ">"); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s>%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, "<"); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s<%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, ">="); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s>=%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, "<="); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s<=%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, "not"); x ] ->
      "" |> with_context2 (Printf.sprintf "!%s" (compile2 x))
  | RBList [ Atom (_, "="); a; b ] ->
      ""
      |> with_context2
           (Printf.sprintf "Objects.equals(%s,%s)" (compile2 a) (compile2 b))
  | RBList (Atom (_, "vector") :: xs) ->
      let args_init =
        xs
        |> List.map (fun x -> compile_ context x |> snd)
        |> List.filter (fun x -> x <> "")
        |> List.reduce_opt ( ^ ) |> Option.value ~default:""
      in
      let args =
        xs |> List.map compile2
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let out_var = Printf.sprintf "List.of(%s)" args in
      args_init |> with_context2 out_var
  | RBList (Atom (_, "hash-map") :: xs) ->
      let args_init =
        xs
        |> List.map (fun x -> compile_ context x |> snd)
        |> List.filter (fun x -> x <> "")
        |> List.reduce_opt ( ^ ) |> Option.value ~default:""
      in
      let args =
        xs |> List.map compile2
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let out_var = Printf.sprintf "Map.of(%s)" args in
      args_init |> with_context2 out_var
  | RBList [ Atom (_, "if"); c; a; b ] ->
      let c_ctx, c_stmt = compile_ context c in
      let a_ctx, a_stmt = compile_ context a in
      let b_ctx, b_stmt = compile_ context b in
      let out_var = NameGenerator.get_new_var () in
      Printf.sprintf "%sfinal Object %s;if(%s){%s%s=%s;}else{%s%s=%s;}" c_stmt
        out_var c_ctx.out_var a_stmt out_var a_ctx.out_var b_stmt out_var
        b_ctx.out_var
      |> with_context2 out_var
  | RBList (Atom (_, "let*") :: SBList vals :: body) ->
      let rec parse_vals nodes =
        match nodes with
        | Atom (m, val_name) :: val_body :: remain ->
            let ctx2, s = compile_ context val_body in
            let type_ =
              if m.symbol = "" then "" else Printf.sprintf "(%s)" m.symbol
            in
            Printf.sprintf "%sfinal var %s=%s%s;%s" s val_name type_
              ctx2.out_var (parse_vals remain)
        | [] -> ""
        | xs -> fail_node xs
      in
      let svals = parse_vals vals in
      let out_var = NameGenerator.get_new_var () in
      let sbody =
        body
        |> List.map (compile_ context)
        |> List.rev
        |> List.mapi (fun i (ctx2, x) ->
               if i = 0 then
                 Printf.sprintf "%sfinal var %s=%s;" x out_var ctx2.out_var
               else x)
        |> List.rev |> String.concat ""
      in
      Printf.sprintf "%s%s" svals sbody |> with_context2 out_var
  (* Lambda *)
  | RBList (Atom (_, "fn*") :: SBList args :: body) ->
      let get_type am = if am.symbol = "" then "" else am.symbol ^ " " in
      let sargs =
        args
        |> List.map (function
             | Atom (am, aname) -> Printf.sprintf "%s%s" (get_type am) aname
             | x -> fail_node [ x ])
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let rec loop_body = function
        | [] -> fail_node []
        | [ x ] ->
            let ctx2, s = compile_ context x in
            let s = if s = "" then "" else s in
            (Printf.sprintf "%s" s, ctx2)
        | x :: xs ->
            let _, s = compile_ context x in
            let s2, ctx2 = loop_body xs in
            (Printf.sprintf "%s%s" s s2, ctx2)
      in
      let sbody, ctx2 = loop_body body in
      let out_var =
        Printf.sprintf
          "(%s)->{try{%sreturn %s;}catch(Exception e){throw new \
           RuntimeException(e);}}"
          sargs sbody ctx2.out_var
      in
      "" |> with_context2 out_var
  (* Function defenition *)
  | RBList
      [
        Atom (_, "def");
        Atom (fname_meta, fname);
        RBList (Atom (_, "fn*") :: SBList args :: body);
      ] ->
      let get_type am = if am.symbol = "" then "Object" else am.symbol in
      let sargs =
        args
        |> List.map (function
             | Atom (am, aname) ->
                 Printf.sprintf "final %s %s" (get_type am) aname
             | x -> fail_node [ x ])
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let rec loop_body = function
        | [] -> fail_node []
        | [ x ] ->
            let ctx2, s = compile_ context x in
            let s = if s = "" then "" else s in
            (Printf.sprintf "%s" s, ctx2)
        | x :: xs ->
            let _, s = compile_ context x in
            let s2, ctx2 = loop_body xs in
            (Printf.sprintf "%s%s" s s2, ctx2)
      in
      let sbody, ctx2 = loop_body body in
      Printf.sprintf
        "public static %s %s(%s){try{%sreturn %s;}catch(Exception e){throw new \
         RuntimeException(e);}}"
        (get_type fname_meta) fname sargs sbody ctx2.out_var
      |> with_context
  | RBList (Atom (_, "module") :: body) ->
      body |> List.map compile
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> Printf.sprintf "class Application {%s}"
      |> with_context
  (* Interop method call *)
  | RBList (Atom (_, ".") :: target :: Atom (_, mname) :: args) ->
      let args2 = args |> List.map (compile_ context) in
      let sargs =
        match args2 with
        | [] -> ""
        | args ->
            args
            |> List.map (fun (ctx2, _) -> ctx2.out_var)
            |> List.reduce (Printf.sprintf "%s,%s")
            |> Printf.sprintf "%s"
      in
      let args_init =
        match args2 with
        | [] -> ""
        | args2 ->
            args2 |> List.map snd
            |> List.filter (fun x -> x <> "")
            |> List.reduce_opt (Printf.sprintf "%s;%s")
            |> Option.value ~default:""
      in
      let mname, is_void =
        if String.ends_with ~suffix:"!" mname then
          (String.sub mname 0 (String.length mname - 1), true)
        else (mname, false)
      in
      if is_void then
        with_context2 "(Void)null"
          (args_init ^ Printf.sprintf "%s.%s(%s);" (compile2 target) mname sargs)
      else
        with_context2
          (Printf.sprintf "%s.%s(%s)" (compile2 target) mname sargs)
          args_init
  (* Constructor *)
  | RBList (Atom (_, "new") :: Atom (_, cnst_name) :: args) ->
      let a =
        if List.length args = 0 then ""
        else args |> List.map compile2 |> List.reduce (Printf.sprintf "%s,%s")
      in
      let out_var = Printf.sprintf "new %s(%s)" cnst_name a in
      "" |> with_context2 out_var
  (* Function call *)
  | RBList (fname :: args) ->
      let stms, sargs =
        if List.length args = 0 then ("", "")
        else
          let xs = args |> List.map (fun x -> compile_ context x) in
          ( xs
            |> List.map (fun (_, stm) -> stm)
            |> List.reduce (Printf.sprintf "%s%s"),
            xs
            |> List.map (fun (ctx2, _) -> ctx2.out_var)
            |> List.reduce (Printf.sprintf "%s,%s") )
      in
      let clear_fname =
        let ctx2, _ = compile_ context fname in
        String.map (function '/' -> '.' | x -> x) ctx2.out_var
      in
      let out_var = Printf.sprintf "%s%s(%s)" stms clear_fname sargs in
      "" |> with_context2 out_var
  (* Default *)
  | x -> fail_node [ x ]

let main (filename : string) code =
  let prelude_macros = {|(defmacro not= [a b] (list 'not (list '= a b)))|} in
  let prefix_lines_count =
    String.fold_left
      (fun acc c -> if c = '\n' then acc + 1 else acc)
      1 prelude_macros
  in
  String.concat "\n" [ prelude_macros; code ]
  |> Frontend.parse_and_simplify prefix_lines_count filename
  |> fun (ctx, exp) ->
  (* print_endline (show_cljexp exp); *)
  let ctx, result = compile_ ctx exp in
  result ^ ctx.out_var |> String.trim
