open Frontend
module StringMap = Map.Make (String)

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

let resolve_exports prelude_code name : exports =
  let code = Effect.perform (ResolveFile name) in
  let code = prelude_code ^ "\n" ^ code in
  let node = parse_and_simplify 0 name code |> snd in

  let rec resolve_loop (exports : string list) = function
    | RBList (Atom (_, "module") :: children) ->
        children |> List.fold_left resolve_loop exports
    | RBList (Atom (_, "def") :: Atom (_, name) :: _) -> name :: exports
    | RBList (Atom (_, "ns") :: _) -> exports
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
}

let rec lint' (ctx : lint_ctx) (node : cljexp) : cljexp * lint_ctx =
  match node with
  | RBList (Atom (_, "ns") :: _ :: depencencies) ->
      let aliases =
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
                          (target, alias)
                      | _ -> failnode "REQ" requiries)
             | x -> failnode "DEP" [ x ])
        |> List.fold_left
             (fun acc (pkg, alias) -> StringMap.add alias pkg acc)
             ctx.aliases
      in
      (node, { ctx with aliases })
  | Atom (l, name) as a
    when String.contains name '/'
         && (not (String.starts_with ~prefix:"\"" name))
         && name <> "/"
         && not (String.starts_with ~prefix:"RT/" name) ->
      let parts = String.split_on_char '/' name in
      let alias = List.nth parts 0 in
      if not (StringMap.mem alias ctx.aliases) then
        failwith
          (Printf.sprintf "Can't find require %s used in %s:%i:%i" alias
             ctx.filename l.line l.pos);

      let extenal_file = StringMap.find alias ctx.aliases in
      (if not (String.starts_with ~prefix:"js." extenal_file) then
         let exports = resolve_exports ctx.prelude_code extenal_file in
         let reference = List.nth parts 1 in
         if not (List.mem reference exports.exports) then
           failwith
             (Printf.sprintf
                "Can't resolve reference '%s/%s' to file '%s.clj' used in \
                 %s:%i:%i"
                alias reference extenal_file ctx.filename l.line l.pos));

      (a, ctx)
  (* | RBList (Atom (_, "module") :: xs) ->
         let rec loop : cljexp list -> cljexp = function
           | RBList [ Atom (l, "def"); name; value ] :: tail ->
               RBList [ Atom (l, "let*"); SBList [ name; value ]; loop tail ]
           | RBList [ Atom (_, "comment") ] :: tail -> loop tail
           | [ x ] -> x
           | [] -> CBList []
           | x -> fail_node x
         in
         xs |> loop |> cljexp_to_json *)
  | Atom _ as a -> (a, ctx)
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
  lint' { aliases = StringMap.empty; filename; prelude_code } node |> fst
