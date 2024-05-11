open Common

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

let read_exports_from_file interpreter prelude_code name : exports =
  let code = Effect.perform (ResolveFile name) in
  let code = prelude_code ^ "\n" ^ code in
  let node =
    Frontend.parse_and_simplify { empty_context with interpreter } name code
    |> snd
  in

  let rec resolve_loop (exports : string list) = function
    | RBList (Atom (_, "module") :: children) ->
        children |> List.fold_left resolve_loop exports
    | RBList (Atom (_, "def") :: Atom (_, name) :: _) -> name :: exports
    | RBList (Atom (_, "ns") :: _) -> exports
    | RBList (Atom (_, "comment") :: _) -> exports
    | RBList
        [
          Atom (_, "__raw_template");
          Atom (_, "\"export default \"");
          RBList [ Atom (_, "quote"); CBList def_exps ];
        ] ->
        let rec loop exports = function
          | [] -> exports
          | Atom (_, k) :: _ :: tail ->
              ("default." ^ String.sub k 1 (String.length k - 1))
              :: loop exports tail
          | n -> failnode __LOC__ n
        in
        loop exports def_exps
    | n -> failnode __LOC__ [ n ]
  in
  { exports = resolve_loop [] node }

type lint_ctx = {
  aliases : string StringMap.t;
  filename : string;
  prelude_code : string;
  (* scope : (cljexp * context) StringMap.t; *)
  local_defs : (int * unit) StringMap.t;
  interpreter : context -> cljexp -> context * cljexp;
  recursion : int;
}
[@@deriving show]

let rec to_pairs = function
  | [] -> []
  | k :: v :: tail -> (k, v) :: to_pairs tail
  | _ -> failwith "Not valid size of list"

let rec lint' (ctx : lint_ctx) (node : cljexp) : cljexp * lint_ctx =
  (* if ctx.recursion > 100 then failnode __LOC__ [ node ];
     let ctx = { ctx with recursion = ctx.recursion + 1 } in *)
  match node with
  | Atom (_, v) when String.get v 0 >= '0' && String.get v 0 <= '9' ->
      (node, ctx)
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
                      | _ -> failnode __LOC__ requiries)
             | RBList (Atom (_, ":import") :: imports) ->
                 imports
                 |> List.concat_map (function
                      | SBList classes ->
                          classes
                          |> List.map (function
                               | Atom (_, cls) -> ("", cls, Some cls)
                               | _ -> failnode __LOC__ classes)
                      | n -> failnode __LOC__ [ n ])
             | x -> failnode __LOC__ [ x ])
        |> List.fold_left
             (fun ctx (pkg, alias, local_val) ->
               let local_defs =
                 match local_val with
                 | None -> ctx.local_defs
                 | Some x -> StringMap.add x (0, ()) ctx.local_defs
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
         && (not (String.contains name '.'))
         && (not (String.starts_with ~prefix:"\"" name))
         && name <> "/"
         && (not (String.starts_with ~prefix:"RT/" name))
         && (not (String.starts_with ~prefix:"y2k.RT/" name))
         (* FIXME: this is a hack for java compilation *)
         && (not (String.starts_with ~prefix:"String/" name))
         && not (String.starts_with ~prefix:"ClassLoader/" name) ->
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
         let exports =
           read_exports_from_file ctx.interpreter ctx.prelude_code extenal_file
         in
         let reference = List.nth parts 1 in
         if not (List.mem reference exports.exports) then
           failwith
             (Printf.sprintf
                "Can't resolve reference '%s/%s' to file '%s.clj' used in \
                 %s:%i:%i"
                alias reference extenal_file ctx.filename l.line l.pos));

      (a, ctx)
  (* Check local variable *)
  | Atom (l, vname)
    when (not (String.starts_with ~prefix:"\"" vname))
         && "." <> vname
         && (not (String.contains vname '/'))
         && (not (String.contains vname '.'))
         && (not (String.starts_with ~prefix:"'" vname))
         && (not (String.starts_with ~prefix:":" vname))
         && (not (String.starts_with ~prefix:"-" vname))
         && (not (String.starts_with ~prefix:"RT/" vname))
         && (not (String.starts_with ~prefix:"y2k.RT/" vname))
         && (String.get vname 0 < '0' || String.get vname 0 > '9') ->
      let parts = String.split_on_char '.' vname in
      let parts = String.split_on_char '?' (List.nth parts 0) in
      let fname = List.nth parts 0 in
      if
        (not (StringMap.mem fname ctx.local_defs))
        (* TODO: убрать хардкод *)
        && vname <> "console"
        && vname <> "fx!"
        && vname <> "gen-class*" && vname <> "set!" && vname <> "alert"
        && vname <> "Promise" && vname <> "RegExp" && vname <> "JSON"
        && vname <> "process" && vname <> "String" && vname <> "is*"
        && vname <> "as*" && vname <> "list" && vname <> "vector"
        && vname <> "vec" && vname <> "=" && vname <> "str" && vname <> "get"
        && vname <> "concat" && vname <> "parseInt" && vname <> "while"
        && vname <> "+" && vname <> "-" && vname <> "*" && vname <> "/"
        && vname <> ">" && vname <> ">=" && vname <> "<" && vname <> "if"
        && vname <> "<=" && vname <> "not" && vname <> "document"
        && vname <> "true" && vname <> "false" && vname <> "setTimeout"
        && vname <> "eval" && vname <> "fetch" && vname <> "Array"
        && vname <> "unit" && vname <> "try" && vname <> "assoc"
        && vname <> "ignore" && vname <> "null" && vname <> "module"
        && vname <> "new" && vname <> "false" && vname <> "__raw_template"
        && vname <> "window"
      then (
        prerr_endline @@ show_lint_ctx ctx;
        failwith
          (Printf.sprintf "[%s] Can't find variable [%s|%s] used from %s:%i:%i"
             __LOC__ fname vname ctx.filename l.line l.pos));
      (node, ctx)
  | Atom _ as a -> (a, ctx)
  | RBList (Atom (_, "let*") :: SBList binding :: body) ->
      let ctx =
        binding |> to_pairs
        |> List.fold_left
             (fun ctx (key, value) ->
               lint' ctx value |> ignore;
               let key =
                 match key with Atom (_, x) -> x | x -> failnode __LOC__ [ x ]
               in
               {
                 ctx with
                 local_defs = StringMap.add key (-1, ()) ctx.local_defs;
               })
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
                 match x with Atom (_, x) -> x | x -> failnode __LOC__ [ x ]
               in
               {
                 ctx with
                 local_defs = StringMap.add key (-1, ()) ctx.local_defs;
               })
             ctx
      in
      let ctx = body |> List.fold_left (fun ctx n -> lint' ctx n |> snd) ctx in
      (node, ctx)
  | RBList (Atom (_, "catch") :: _ :: Atom (_, err_val) :: body) ->
      let ctx2 =
        { ctx with local_defs = StringMap.add err_val (0, ()) ctx.local_defs }
      in
      body |> List.fold_left (fun ctx n -> lint' ctx n |> snd) ctx2 |> ignore;
      (node, ctx)
  | RBList
      [
        Atom (_, "def");
        Atom (_, name);
        (RBList (Atom (_, "fn*") :: SBList args :: _) as fn);
      ] ->
      let rec compute_count = function
        | Atom (_, "&") :: _ -> 1000
        | _ :: rest -> 1 + compute_count rest
        | [] -> 0
      in
      let local_ctx =
        {
          ctx with
          local_defs =
            StringMap.add name (compute_count args, ()) ctx.local_defs;
        }
      in
      [ fn ]
      |> List.fold_left (fun ctx n -> lint' ctx n |> snd) local_ctx
      |> ignore;
      (node, local_ctx)
  | RBList [ Atom (_, "def"); Atom (_, name); body ] ->
      let local_ctx =
        { ctx with local_defs = StringMap.add name (-1, ()) ctx.local_defs }
      in
      lint' local_ctx body |> ignore;
      (node, local_ctx)
  | RBList (Atom (_, "module") :: body) ->
      body
      |> List.fold_left_map
           (fun ctx n ->
             let _, ctx2 = lint' ctx n in
             (ctx2, ()))
           ctx
      |> ignore;
      (node, ctx)
  (* Check function call arguments count *)
  | RBList (Atom (m, fname) :: args)
    when (not (String.contains fname '/'))
         && (not (String.contains fname '.'))
         (* TODO: убрать хардкод *)
         && fname <> "fetch"
         && fname <> "fx!" && fname <> "gen-class*" && fname <> "set!"
         && fname <> "alert" && fname <> "RegExp" && fname <> "is*"
         && fname <> "=" && fname <> "as*" && fname <> "list" && fname <> "vec"
         && fname <> "vector" && fname <> "str" && fname <> "concat"
         && fname <> "get" && fname <> "parseInt" && fname <> "+"
         && fname <> "-" && fname <> "*" && fname <> ">" && fname <> "if"
         && fname <> ">=" && fname <> "<" && fname <> "<=" && fname <> "not"
         && fname <> "while" && fname <> "setTimeout" && fname <> "eval"
         && fname <> "ignore" && fname <> "try" && fname <> "assoc"
         && fname <> "." && fname <> "new" && fname <> "__raw_template" ->
      let fname =
        if String.starts_with ~prefix:"'" fname then
          String.sub fname 1 (String.length fname - 1)
        else fname
      in
      let arg_count, _ =
        ctx.local_defs |> StringMap.find_opt fname |> function
        | Some x -> x
        | None ->
            Printf.sprintf "%s: Can't find function '%s'" __LOC__ fname
            |> failwith
      in
      if arg_count < 0 then ()
      else if arg_count >= 1000 && List.length args < arg_count mod 1000 then
        Printf.sprintf "[%s] %s: '%s' expected at least %d arguments, got %d"
          (show_error_location ctx.filename m)
          __LOC__ fname (arg_count mod 1000) (List.length args)
        |> failwith
      else if arg_count < 1000 && arg_count <> List.length args then
        Printf.sprintf "[%s] %s: '%s' expected %d arguments, got %d"
          (show_error_location ctx.filename m)
          __LOC__ fname arg_count (List.length args)
        |> failwith;
      (node, ctx)
  | RBList xs as origin ->
      let ctx = xs |> List.fold_left (fun ctx n -> lint' ctx n |> snd) ctx in
      (origin, ctx)
  | SBList xs as origin ->
      let ctx = xs |> List.fold_left (fun ctx n -> lint' ctx n |> snd) ctx in
      (origin, ctx)
  | CBList xs as origin ->
      let ctx = xs |> List.fold_left (fun ctx n -> lint' ctx n |> snd) ctx in
      (origin, ctx)

let lint interpreter prelude_code filename node =
  (* print_endline "==================================================";
     print_endline (show_cljexp node); *)
  let prelude_lint_ctx =
    prelude_code
    |> Frontend.parse_and_simplify { empty_context with interpreter } "prelude"
    |> snd
    |> lint'
         {
           aliases = StringMap.empty;
           filename;
           prelude_code;
           local_defs = StringMap.empty;
           interpreter;
           recursion = 0;
         }
    |> snd
  in
  lint'
    {
      aliases = StringMap.empty;
      filename;
      prelude_code;
      local_defs = prelude_lint_ctx.local_defs;
      interpreter;
      recursion = 0;
    }
    node
  |> fst
