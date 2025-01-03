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
          (* *)
          (* full_name ^ String.sub name i (String.length name - i)) *)
          c ^ "." ^ f_name)

let rec invoke (ctx : inner_context) (node : cljexp) : inner_context * cljexp =
  match node with
  | Atom _ as x -> (ctx, x)
  | RBList (m, (Atom (_, "do*") as do_) :: body) ->
      let ctx, body = List.fold_left_map invoke ctx body in
      (ctx, RBList (m, do_ :: body))
  | RBList (m, [ (Atom (_, "ns") as ns_); RBList (_, [ (Atom (_, "quote*") as q); RBList (_, pkg_name :: body) ]) ]) ->
      let ctx, body =
        List.fold_left
          (fun (ctx, body) n ->
            match n with
            | RBList (_, Atom (_, ":require") :: rs) ->
                let ctx =
                  List.fold_left
                    (fun ctx x ->
                      match x with
                      | SBList (_, [ Atom (_, full_name); _; Atom (_, alias) ]) ->
                          { ctx with requires = StringMap.add alias full_name ctx.requires }
                      | n -> failnode __LOC__ [ n ])
                    ctx rs
                in
                (ctx, body)
            | RBList (_, Atom (_, ":import") :: _) as n -> (ctx, body @ [ n ])
            | n -> failnode __LOC__ [ n ])
          (ctx, []) body
      in
      (ctx, RBList (m, [ ns_; RBList (unknown_location, [ q; RBList (unknown_location, pkg_name :: body) ]) ]))
  (* Special forms *)
  | RBList (m, (Atom (_, "fn*") as fn) :: args :: body) ->
      let ctx, body = List.fold_left_map invoke ctx body in
      (ctx, RBList (m, fn :: args :: body))
  | RBList (_, [ Atom (_, "def*"); _ ]) -> (ctx, node)
  | RBList (m, [ (Atom (_, "def*") as def); name; value ]) ->
      let ctx, value = invoke ctx value in
      (ctx, RBList (m, [ def; name; value ]))
  | RBList (_, Atom (_, "quote*") :: _) -> (ctx, node)
  | RBList (m, [ (Atom (_, "let*") as let_); name; value ]) ->
      let ctx, value = invoke ctx value in
      (ctx, RBList (m, [ let_; name; value ]))
  (* if *)
  | RBList (m, [ (Atom (_, "if*") as if_); cond; then_; else_ ]) ->
      let _, cond = invoke ctx cond in
      let _, then_ = invoke ctx then_ in
      let _, else_ = invoke ctx else_ in
      (ctx, RBList (m, [ if_; cond; then_; else_ ]))
  (* Catch unhandled special forms *)
  | RBList (_, Atom (_, name) :: _) when name <> "*" && String.ends_with ~suffix:"*" name -> failnode __LOC__ [ node ]
  | RBList (mr, Atom (m, name) :: args) ->
      let name = fix_name ctx name in
      let ctx, args = List.fold_left_map invoke ctx args in
      (ctx, RBList (mr, Atom (m, name) :: args))
  | n ->
      print_endline @@ show_inner_context ctx;
      failnode __LOC__ [ n ]

let main (context : context) (node : cljexp) : cljexp =
  let _, node = invoke { requires = StringMap.empty; context } node in
  node
