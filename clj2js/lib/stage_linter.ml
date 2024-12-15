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
  match (String.index_opt name '.', String.index_opt name '?') with
  | None, None -> name
  | Some i, None -> String.sub name 0 i
  | None, Some n -> String.sub name 0 n
  | Some i, Some n -> String.sub name 0 (min i n)

let invoke (code_ctx : context) prelude_node node =
  let prelude_node = Stage_normalize_bracket.invoke prelude_node in
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
    | RBList
        [
          Atom (_, "def*"); Atom (_, k); (RBList (Atom (_, "fn*") :: _) as fn_);
        ] as x ->
        let ctx = { scope = StringMap.add k () ctx.scope } in
        let ctx, _ = invoke ctx fn_ in
        (ctx, x)
    | RBList (Atom (_, "def*") :: Atom (_, k) :: _) as x ->
        ({ scope = StringMap.add k () ctx.scope }, x)
    | RBList (Atom (_, "let*") :: Atom (_, k) :: _) as x ->
        ({ scope = StringMap.add k () ctx.scope }, x)
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
    | RBList
        [ Atom (_, "ns"); RBList [ Atom (_, "quote*"); RBList (_ :: body) ] ] as
      n ->
        let scope =
          body
          |> List.concat_map (function
               | RBList (Atom (_, ":import") :: body) ->
                   body
                   |> List.concat_map (function
                        | SBList (_ :: classes) -> classes
                        | n -> failnode __LOC__ [ n ])
               | _ -> [])
          |> List.fold_left
               (fun scope cls ->
                 match cls with
                 | Atom (_, cls_name) -> StringMap.add cls_name () scope
                 | n -> failnode __LOC__ [ n ])
               ctx.scope
        in
        ({ scope }, n)
    | RBList body ->
        List.map (fun x -> invoke ctx x) body |> ignore;
        (ctx, node)
    (* Constants *)
    | Atom (_, ".") -> (ctx, node)
    | Atom (_, "nil") -> (ctx, node)
    | Atom (_, name) when String.contains name '/' -> (ctx, node)
    | Atom (_, name) when String.get name 0 = ':' -> (ctx, node)
    | Atom (_, name) when String.get name 0 = '\"' -> (ctx, node)
    | Atom (_, name)
      when (String.get name 0 >= '0' && String.get name 0 <= '9')
           || String.get name 0 = '-' ->
        (ctx, node)
    (* Variables *)
    | Atom (m, full_name) as n ->
        let name = get_var_name full_name in
        if ctx.scope |> StringMap.mem name |> not then (
          prerr_endline @@ debug_show_linter_context ctx;
          prerr_endline @@ "Not found: " ^ name ^ " (" ^ full_name ^ ")" ^ " in "
          ^ code_ctx.filename ^ ":" ^ string_of_int m.line ^ ":"
          ^ string_of_int m.pos;
          failwith __LOC__ |> ignore);
        (ctx, n)
    | node ->
        failnode (__LOC__ ^ " - " ^ debug_show_linter_context ctx) [ node ]
  in
  RBList [ Atom (unknown_location, "do*"); prelude_node; node ]
  |> invoke { scope = StringMap.empty }
  |> ignore;
  node
