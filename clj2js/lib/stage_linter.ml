open Common

let yojson_of_stringmap m =
  StringMap.bindings m |> [%to_yojson: (string * unit) list]

type linter_context = {
  scope : unit StringMap.t; [@to_yojson yojson_of_stringmap]
}
[@@deriving to_yojson]

let debug_show_linter_context (ctx : linter_context) =
  linter_context_to_yojson ctx |> Yojson.Safe.pretty_to_string

let get_var_name name =
  match String.index_opt name '.' with
  | None -> name
  | Some i -> String.sub name 0 i

let invoke prelude_node node =
  let rec invoke (ctx : linter_context) (node : cljexp) :
      linter_context * cljexp =
    (* print_endline @@ "== LOG == " ^ debug_show_cljexp [ node ]; *)
    match node with
    | RBList (Atom (_, "fn*") :: RBList args :: body) ->
        let scope =
          List.fold_left
            (fun scope a ->
              let a =
                match a with Atom (_, x) -> x | x -> failnode __LOC__ [ x ]
              in
              StringMap.add a () scope)
            ctx.scope args
        in
        let _ = List.fold_left_map invoke { scope } body in
        (ctx, node)
    | RBList [ Atom (_, "if*"); cond; then_; else_ ] as n ->
        invoke ctx cond |> ignore;
        let scope =
          ctx.scope
          |> StringMap.merge
               (fun _ l r -> match (l, r) with _, Some _ -> r | l, _ -> l)
               (fst (invoke ctx then_)).scope
          |> StringMap.merge
               (fun _ l r -> match (l, r) with _, Some _ -> r | l, _ -> l)
               (fst (invoke ctx else_)).scope
        in
        ({ scope }, n)
    | RBList [ Atom (_, "quote*"); _ ] -> (ctx, node)
    | RBList (Atom (_, "do*") :: body) ->
        let ctx, _ = List.fold_left_map invoke ctx body in
        (ctx, node)
    | RBList (Atom (_, "def*") :: Atom (_, k) :: _) as x ->
        ({ scope = StringMap.add k () ctx.scope }, x)
    | RBList (Atom (_, "let*") :: Atom (_, k) :: _) as x ->
        ({ scope = StringMap.add k () ctx.scope }, x)
    (* | RBList (Atom (_, "bind*") :: Atom (_, k) :: _) as x ->
        ({ scope = StringMap.add k () ctx.scope }, x) *)
    (* | RBList (Atom (_, "bind-update*") :: _ :: _) as x -> (ctx, x) *)
    (* TODO: delete this form *)
    | RBList (Atom (_, "let*") :: SBList bindings :: tail) as x ->
        let rec loop ctx = function
          | [] -> ctx
          | Atom (_, k) :: _ :: tail ->
              let ctx = { scope = StringMap.add k () ctx.scope } in
              loop ctx tail
          | n -> failnode __LOC__ n
        in
        let ctx = loop ctx bindings in
        let ctx, _ = List.fold_left_map invoke ctx tail in
        (ctx, x)
    | RBList body ->
        List.map (fun x -> invoke ctx x) body |> ignore;
        (ctx, node)
    (* Constants *)
    | Atom (_, ".") -> (ctx, node)
    | Atom (_, "nil") -> (ctx, node)
    | Atom (_, name) when String.get name 0 = ':' -> (ctx, node)
    | Atom (_, name) when String.get name 0 = '\"' -> (ctx, node)
    | Atom (_, name) when String.get name 0 >= '0' && String.get name 0 <= '9'
      ->
        (ctx, node)
    | Atom (_, name) when String.contains name '/' -> (ctx, node)
    (* Variables *)
    | Atom (_, name) as n ->
        let name = get_var_name name in
        if ctx.scope |> StringMap.mem name |> not then (
          prerr_endline @@ "Not found: " ^ name;
          prerr_endline @@ debug_show_linter_context ctx;
          failnode __LOC__ [ n ] |> ignore);
        (ctx, n)
    | node ->
        failnode (__LOC__ ^ " - " ^ debug_show_linter_context ctx) [ node ]
  in
  RBList [ Atom (unknown_location, "do*"); prelude_node; node ]
  |> invoke { scope = StringMap.empty }
  |> ignore;
  node
