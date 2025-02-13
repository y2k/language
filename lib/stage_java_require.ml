open Common

type inner_context = { requires : string StringMap.t; context : context } [@@deriving show]

let merge_packages (context : context) (rel_ns : string) =
  let rec loop xs ps =
    (* prerr_endline @@ "LOG3: " ^ String.concat "," (xs @ ps); *)
    match (xs, ps) with
    | _ :: xs, ".." :: ps -> loop xs ps
    | xs, "." :: ps -> loop xs ps
    | xs, ps -> List.rev xs @ ps
  in
  loop (String.split_on_char '.' context.base_ns |> List.rev) (String.split_on_char '/' rel_ns) |> String.concat "."

let fix_name { requires; context } name =
  (* prerr_endline @@ "LOG: " ^ name ^ " " ^ show_inner_context { requires }; *)
  match String.index_opt name '/' with
  | None -> name
  | Some i -> (
      let alias = String.sub name 0 i in
      match StringMap.find_opt alias requires with
      | None -> name
      | Some full_name ->
          let f_name = String.sub name (i + 1) (String.length name - i - 1) in
          let a = String.sub full_name 1 (String.length full_name - 2) in
          (* print_endline @@ "LOG1: " ^ a ^ " | " ^ name; *)
          let b = merge_packages context a in
          let c = b |> String.map (function '/' -> '.' | x -> x) in
          (* print_endline @@ "LOG2: " ^ a ^ " | " ^ b ^ " | " ^ c; *)
          c ^ "." ^ f_name)

let rec invoke (ctx : inner_context) (node : sexp) : inner_context * sexp =
  match node with
  | SAtom _ as x -> (ctx, x)
  | SList (m, (SAtom (_, "do*") as do_) :: body) ->
      let ctx, body = List.fold_left_map invoke ctx body in
      (ctx, SList (m, do_ :: body))
  | SList (m, [ (SAtom (_, "ns") as ns_); SList (_, [ (SAtom (_, "quote*") as q); SList (_, pkg_name :: body) ]) ]) ->
      let ctx, body =
        List.fold_left
          (fun (ctx, body) n ->
            match n with
            | SList (_, SAtom (_, ":require") :: rs) ->
                let ctx =
                  List.fold_left
                    (fun ctx x ->
                      match x with
                      | SList (_, [ SAtom (_, full_name); _; SAtom (_, alias) ]) ->
                          { ctx with requires = StringMap.add alias full_name ctx.requires }
                      | n -> failsexp __LOC__ [ n ])
                    ctx rs
                in
                (ctx, body)
            | SList (_, SAtom (_, ":import") :: _) as n -> (ctx, body @ [ n ])
            | n -> failsexp __LOC__ [ n ])
          (ctx, []) body
      in
      (ctx, SList (m, [ ns_; SList (unknown_location, [ q; SList (unknown_location, pkg_name :: body) ]) ]))
  (* Special forms *)
  | SList (m, (SAtom (_, "fn*") as fn) :: args :: body) ->
      let ctx, body = List.fold_left_map invoke ctx body in
      (ctx, SList (m, fn :: args :: body))
  | SList (_, [ SAtom (_, "def*"); _ ]) -> (ctx, node)
  | SList (m, [ (SAtom (_, "def*") as def); name; value ]) ->
      let ctx, value = invoke ctx value in
      (ctx, SList (m, [ def; name; value ]))
  | SList (_, SAtom (_, "quote*") :: _) -> (ctx, node)
  | SList (m, [ (SAtom (_, "let*") as let_); name; value ]) ->
      let ctx, value = invoke ctx value in
      (ctx, SList (m, [ let_; name; value ]))
  (* if *)
  | SList (m, [ (SAtom (_, "if*") as if_); cond; then_; else_ ]) ->
      let _, cond = invoke ctx cond in
      let _, then_ = invoke ctx then_ in
      let _, else_ = invoke ctx else_ in
      (ctx, SList (m, [ if_; cond; then_; else_ ]))
  (* Catch unhandled special forms *)
  | SList (_, SAtom (_, name) :: _) when name <> "*" && String.ends_with ~suffix:"*" name -> failsexp __LOC__ [ node ]
  (* Function call *)
  | SList (mr, SAtom (m, name) :: args) ->
      let name = fix_name ctx name in
      let ctx, args = List.fold_left_map invoke ctx args in
      (ctx, SList (mr, SAtom (m, name) :: args))
  | SList (mr, args) ->
      let ctx, args = List.fold_left_map invoke ctx args in
      (ctx, SList (mr, args))

let main (context : context) (node : sexp) : sexp =
  let _, node = invoke { requires = StringMap.empty; context } node in
  node
