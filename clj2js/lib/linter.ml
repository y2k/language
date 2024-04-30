open Frontend

type exports = { exports : string list }
type _ Effect.t += ResolveFile : string -> string Effect.t

let run_resolve f2 f =
  let open Effect.Deep in
  Effect.Deep.try_with f ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | ResolveFile path ->
              let (result : string) = f2 path in
              Some (fun (k : (a, _) continuation) -> continue k result)
          | _ -> None);
    }

let read_exports_from_file prelude_code name : exports =
  let code = Effect.perform (ResolveFile name) in
  let code = prelude_code ^ "\n" ^ code in
  let node = Frontend.parse_and_simplify StringMap.empty 0 name code |> snd in

  let rec resolve_loop (exports : string list) = function
    | RBList (Atom (_, "module") :: children) ->
        children |> List.fold_left resolve_loop exports
    | RBList (Atom (_, "def") :: Atom (_, name) :: _) -> name :: exports
    | RBList (Atom (_, "ns") :: _) -> exports
    | RBList (Atom (_, "comment") :: _) -> exports
    | RBList [ Atom (_, "export-default"); RBList (Atom _ :: def_exps) ] ->
        let rec loop exports = function
          | [] -> exports
          | Atom (_, k) :: _ :: tail ->
              ("default." ^ String.sub k 1 (String.length k - 1))
              :: loop exports tail
          | n -> failnode "RSLP_LP" n
        in
        loop exports def_exps
    | n -> failnode "RSLV_EXP" [ n ]
  in
  { exports = resolve_loop [] node }

type lint_ctx = {
  aliases : string StringMap.t;
  filename : string;
  prelude_code : string;
  local_defs : string list;
}

let rec to_pairs = function
  | [] -> []
  | k :: v :: tail -> (k, v) :: to_pairs tail
  | _ -> failwith "Not valid size of list"

let rec lint' (ctx : lint_ctx) (node : cljexp) : cljexp * lint_ctx =
  match node with
  | RBList (Atom (_, "quote") :: _) -> (node, ctx)
  | RBList (Atom (_, "comment") :: _) -> (node, ctx)
  | RBList (Atom (_, "ns") :: _ :: depencencies) ->
      let ctx2 =
        depencencies
        |> List.concat_map (function
             | RBList (Atom (_, ":require") :: requiries) ->
                 requiries
                 |> List.map (function
                      | SBList
                          [
                            Atom (_, package); Atom (_, ":as"); Atom (_, alias);
                          ] ->
                          let target =
                            if String.starts_with ~prefix:"\"" package then
                              String.sub package 1 (String.length package - 2)
                            else package
                          in
                          (target, alias, None)
                      | _ -> failnode "REQ" requiries)
             | RBList [ Atom (_, ":import"); SBList (_ :: classes) ] ->
                 classes
                 |> List.map (function
                      | Atom (_, cls) -> ("", cls, Some cls)
                      | _ -> failnode "LNTDEPIMP" classes)
             | x -> failnode "DEP" [ x ])
        |> List.fold_left
             (fun ctx (pkg, alias, local_val) ->
               let local_defs =
                 match local_val with
                 | None -> ctx.local_defs
                 | Some x -> x :: ctx.local_defs
               in
               {
                 ctx with
                 aliases = StringMap.add alias pkg ctx.aliases;
                 local_defs;
               })
             ctx
      in
      (node, ctx2)
  (* Check external reference *)
  | Atom (l, name) as a
    when String.contains name '/'
         && (not (String.starts_with ~prefix:"\"" name))
         && name <> "/"
         && (not (String.starts_with ~prefix:"RT/" name))
         && (not (String.starts_with ~prefix:"y2k.RT/" name))
         (* FIXME: this is a hack for java compilation *)
         && not (String.starts_with ~prefix:"String/" name) ->
      let parts = String.split_on_char '/' name in
      let alias = List.nth parts 0 in
      if not (StringMap.mem alias ctx.aliases) then
        failwith
          (Printf.sprintf "Can't find require %s used in %s:%i:%i" alias
             ctx.filename l.line l.pos);

      let extenal_file = StringMap.find alias ctx.aliases in
      (if
         extenal_file <> ""
         && not (String.starts_with ~prefix:"js." extenal_file)
       then
         let exports = read_exports_from_file ctx.prelude_code extenal_file in
         let reference = List.nth parts 1 in
         if not (List.mem reference exports.exports) then
           failwith
             (Printf.sprintf
                "Can't resolve reference '%s/%s' to file '%s.clj' used in \
                 %s:%i:%i"
                alias reference extenal_file ctx.filename l.line l.pos));

      (a, ctx)
  (* Check local variable *)
  | Atom (l, fname)
    when (not (String.starts_with ~prefix:"\"" fname))
         && (not (String.contains fname '/'))
         && (not (String.starts_with ~prefix:"'" fname))
         && (not (String.starts_with ~prefix:":" fname))
         && (not (String.starts_with ~prefix:"-" fname))
         && (not (String.starts_with ~prefix:"RT/" fname))
         && (not (String.starts_with ~prefix:"y2k.RT/" fname))
         && (String.get fname 0 < '0' || String.get fname 0 > '9')
         && fname <> "<=" && fname <> ">=" && fname <> ">" && fname <> "<"
         && fname <> "%" && fname <> "*" && fname <> "/" && fname <> "-"
         && fname <> "+" && fname <> "=" && fname <> "if" && fname <> "fn*"
         && fname <> "do" && fname <> "null" && fname <> "let*" && fname <> "."
         && fname <> "false" && fname <> "true" && fname <> "module"
         && fname <> "quote" && fname <> "catch" && fname <> "try"
         && fname <> "while" && fname <> "new" && fname <> "throw" ->
      let parts = String.split_on_char '.' fname in
      let parts = String.split_on_char '?' (List.nth parts 0) in
      let fname = List.nth parts 0 in
      if not (List.mem fname ctx.local_defs) then
        failwith
          (Printf.sprintf "Can't find variable '%s' used from %s:%i:%i" fname
             ctx.filename l.line l.pos);
      (node, ctx)
  | Atom _ as a -> (a, ctx)
  | RBList (Atom (_, "let*") :: SBList binding :: body) ->
      let ctx =
        binding |> to_pairs
        |> List.fold_left
             (fun ctx (key, value) ->
               lint' ctx value |> ignore;
               let key =
                 match key with
                 | Atom (_, x) -> x
                 | x -> failnode "LET*NOT" [ x ]
               in
               { ctx with local_defs = key :: ctx.local_defs })
             ctx
      in
      let ctx = body |> List.fold_left (fun ctx n -> lint' ctx n |> snd) ctx in
      (node, ctx)
  | RBList (Atom (_, "fn*") :: SBList binding :: body) ->
      let ctx =
        binding
        |> List.fold_left
             (fun ctx x ->
               let key =
                 match x with Atom (_, x) -> x | x -> failnode "FN*NOT" [ x ]
               in
               { ctx with local_defs = key :: ctx.local_defs })
             ctx
      in
      let ctx = body |> List.fold_left (fun ctx n -> lint' ctx n |> snd) ctx in
      (node, ctx)
  | RBList (Atom (_, "catch") :: _ :: Atom (_, err_val) :: body) ->
      let ctx2 = { ctx with local_defs = err_val :: ctx.local_defs } in
      body |> List.fold_left (fun ctx n -> lint' ctx n |> snd) ctx2 |> ignore;
      (node, ctx)
  | RBList (Atom (_, "def") :: Atom (_, name) :: body) ->
      let local_ctx = { ctx with local_defs = name :: ctx.local_defs } in
      body
      |> List.fold_left (fun ctx n -> lint' ctx n |> snd) local_ctx
      |> ignore;
      (node, { ctx with local_defs = name :: ctx.local_defs })
  | RBList xs as origin ->
      let ctx = xs |> List.fold_left (fun ctx n -> lint' ctx n |> snd) ctx in
      (origin, ctx)
  | SBList xs as origin ->
      let ctx = xs |> List.fold_left (fun ctx n -> lint' ctx n |> snd) ctx in
      (origin, ctx)
  | CBList xs as origin ->
      let ctx = xs |> List.fold_left (fun ctx n -> lint' ctx n |> snd) ctx in
      (origin, ctx)

let lint prelude_code filename node =
  (* print_endline "==================================================";
     print_endline (show_cljexp node); *)
  let prelude_lint_ctx =
    prelude_code
    |> Frontend.parse_and_simplify StringMap.empty 0 "prelude"
    |> snd
    |> lint'
         { aliases = StringMap.empty; filename; prelude_code; local_defs = [] }
    |> snd
  in
  lint'
    {
      aliases = StringMap.empty;
      filename;
      prelude_code;
      local_defs = prelude_lint_ctx.local_defs;
    }
    node
  |> fst
