let trace prefix to_string x =
  prerr_endline @@ "LOG " ^ Printf.sprintf "%s %s" prefix (to_string x);
  x

module Files = struct
  let realpath ?root path =
    let root = Option.value root ~default:(Sys.getcwd ()) in
    let result = Filename.concat root path in
    result
    |> Str.global_replace (Str.regexp "/\\./") "/"
    |> Str.global_replace (Str.regexp "/[^/\\..]+/\\.\\./") "/"
    |> Str.global_replace (Str.regexp "/[^/\\..]+/\\.\\./") "/"
  (* |> trace "LOG[realpath]" (Printf.sprintf "%s -> %s" path) *)
end

type config = {
  no_lint : bool;
  virtual_src : string;
  log : bool;
  no_deps : bool;
}

let config_default =
  { no_lint = false; virtual_src = ""; log = false; no_deps = false }

module FileReader = struct
  type _ Effect.t += Load : string -> string Effect.t
  type _ Effect.t += Realpath : string -> string Effect.t

  let read filename = Effect.perform (Load filename)
  let realpath path = Effect.perform (Realpath path)

  let with_stub_scope (content : string) f arg =
    let open Effect.Deep in
    Effect.Deep.match_with f arg
      {
        retc = Fun.id;
        exnc =
          (fun e ->
            Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()));
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Load _ ->
                Some (fun (k : (a, _) continuation) -> continue k content)
            | Realpath path ->
                Some
                  (fun (k : (a, _) continuation) ->
                    let path =
                      Str.global_replace (Str.regexp "/\\./") "/" path
                    in
                    continue k path)
            | _ -> None);
      }

  let with_scope f arg =
    let open Effect.Deep in
    Effect.Deep.match_with f arg
      {
        retc = Fun.id;
        exnc =
          (fun e ->
            Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()));
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Load path ->
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k In_channel.(with_open_bin path input_all))
            | Realpath path ->
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k (Files.realpath path))
            | _ -> None);
      }
end

let last xs = List.nth xs (List.length xs - 1)
let butlast xs = List.rev (List.tl (List.rev xs))
let loc (a, b, c, _) = Printf.sprintf "%s:%i:%i" a b c

type meta = { line : int; pos : int; symbol : string } [@@deriving show]

let unknown_location = { line = 0; pos = 0; symbol = "" }
let meta_empty = { line = 0; pos = 0; symbol = "" }

type sexp = SAtom of meta * string | SList of meta * sexp list
[@@deriving show]

type cljexp =
  | Atom of meta * string
  | RBList of meta * cljexp list
  | SBList of meta * cljexp list
  | CBList of meta * cljexp list
[@@deriving show]

let unpack_string x =
  if String.starts_with ~prefix:"\"" x then String.sub x 1 (String.length x - 2)
  else x

let pack_string x = SAtom (meta_empty, "\"" ^ x ^ "\"")
let unpack_symbol x = String.sub x 1 (String.length x - 1)

let get_type meta =
  if meta.symbol = "" || meta.symbol = ":private" then "Object" else meta.symbol

let get_type_or_var meta =
  if meta.symbol = "" || meta.symbol = ":private" then "var" else meta.symbol

let change_meta m node =
  match node with
  | Atom (_, x) -> Atom (m, x)
  | RBList (_, xs) -> RBList (m, xs)
  | CBList (_, xs) -> CBList (m, xs)
  | SBList (_, xs) -> SBList (m, xs)

let change_smeta m (node : sexp) =
  match node with SAtom (_, x) -> SAtom (m, x) | SList (_, xs) -> SList (m, xs)

let get_symbol = function
  | Atom (m, _) -> m.symbol
  | RBList (m, _) -> m.symbol
  | SBList (m, _) -> m.symbol
  | CBList (m, _) -> m.symbol

let get_sexp_symbol = function
  | SAtom (m, _) -> m.symbol
  | SList (m, _) -> m.symbol

let unwrap_do = function RBList (_, Atom (_, "do*") :: xs) -> xs | x -> [ x ]

let unwrap_sexp_do = function
  | SList (_, SAtom (_, "do*") :: xs) -> xs
  | x -> [ x ]

let unwrap_single_do = function
  | RBList (_, [ Atom (_, "do*"); xs ]) -> xs
  | x -> x

module StringMap = struct
  include Map.Make (String)

  let pp pp_value fmt map =
    let bindings = bindings map in
    Format.fprintf fmt "@[<v>{";
    bindings
    |> List.iter (fun (key, value) ->
           Format.fprintf fmt "@,%a -> %a;" Format.pp_print_string key pp_value
             value);
    Format.fprintf fmt "@,}@]"
end

type function_decl = { params : cljexp list; body : cljexp list }
[@@deriving show]

type obj =
  | OVector of meta * obj list
  | OList of meta * obj list
  | OMap of meta * (obj * obj) list
  | OString of meta * string
  | OInt of meta * int
  | OFloat of meta * float
  | OBool of meta * bool
  | ONil of meta
  | OLambda of meta * (obj list -> obj)
  | OQuote of meta * sexp
[@@deriving show]

module Obj = struct
  let failobj loc xs =
    match xs with
    | [ x ] -> Printf.sprintf "%s %s" loc (show_obj x) |> failwith
    | xs ->
        Printf.sprintf "%s %s" loc (show_obj (OList (meta_empty, xs)))
        |> failwith

  let rec equal a b =
    match (a, b) with
    | OInt (_, x), OInt (_, y) -> x = y
    | OString (_, x), OString (_, y) -> x = y
    | OBool (_, x), OBool (_, y) -> x = y
    | OVector (_, xs), OVector (_, ys) -> List.for_all2 equal xs ys
    | ONil _, ONil _ -> true
    | ONil _, _ | _, ONil _ -> false
    | a, b -> failobj __LOC__ [ a; b ]
end

type context = {
  log : bool;
  filename : string;
  loc : meta;
  start_line : int;
  macros : cljexp StringMap.t;
  scope : (obj * context ref) StringMap.t;
  functions : (cljexp * context ref) StringMap.t;
  prelude_scope : unit StringMap.t;
  interpreter : context -> cljexp -> context * cljexp;
  base_ns : string;
  imports : context StringMap.t;
  eval : context -> cljexp -> cljexp;
}
[@@deriving show]

let show_error_location filename m =
  Printf.sprintf "%s:%d:%d" filename m.line m.pos

let debug_show_scope ctx =
  StringMap.bindings ctx.scope
  |> List.map (fun (k, _) -> k)
  |> String.concat ", "

let debug_show_macro ctx =
  StringMap.bindings ctx.macros
  |> List.map (fun (k, _) -> k)
  |> String.concat ", "

let debug_show_imports ctx =
  StringMap.bindings ctx.imports
  |> List.map (fun (k, _) -> k)
  |> String.concat ", "

let empty_context =
  {
    log = false;
    filename = "";
    loc = unknown_location;
    start_line = 0;
    macros = StringMap.empty;
    scope = StringMap.empty;
    prelude_scope = StringMap.empty;
    interpreter = (fun _ _ -> failwith __LOC__);
    base_ns = "";
    imports = StringMap.empty;
    eval = (fun _ _ -> failwith __LOC__);
    functions = StringMap.empty;
  }

module NameGenerator = struct
  type _ Effect.t += CreateVal : string Effect.t

  let with_scope f =
    let index = ref 0 in
    let open Effect.Deep in
    Effect.Deep.match_with f ()
      {
        retc = Fun.id;
        exnc =
          (fun e ->
            Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()));
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | CreateVal ->
                index := !index + 1;
                let result = "p__" ^ string_of_int !index in
                Some (fun (k : (a, _) continuation) -> continue k result)
            | _ -> None);
      }

  let get_new_var () = Effect.perform CreateVal
end

module List = struct
  include List

  let rec split_into_pairs = function
    | a :: b :: rest -> (a, b) :: split_into_pairs rest
    | _ -> []

  let reduce loc f xs =
    match xs with
    | [] -> failwith loc
    | xs -> List.fold_left f (List.hd xs) (List.tl xs)

  let reduce_opt f xs = match xs with [] -> None | xs -> Some (reduce "" f xs)
end

let rec debug_show_cljexp1 = function
  | Atom (m, x) when m.symbol = "" -> x
  | Atom (m, x) -> "^" ^ m.symbol ^ " " ^ x
  | RBList (m, xs) when m.symbol = "" ->
      "(" ^ String.concat " " (List.map debug_show_cljexp1 xs) ^ ")"
  | RBList (m, xs) ->
      "^" ^ m.symbol ^ " ("
      ^ String.concat " " (List.map debug_show_cljexp1 xs)
      ^ ")"
  | SBList (_, xs) ->
      "[" ^ String.concat " " (List.map debug_show_cljexp1 xs) ^ "]"
  | CBList (_, xs) ->
      "{" ^ String.concat " " (List.map debug_show_cljexp1 xs) ^ "}"

let rec debug_show_sexp1 = function
  | SAtom (m, x) when m.symbol = "" -> x
  | SAtom (m, x) -> "^" ^ m.symbol ^ " " ^ x
  | SList (m, xs) when m.symbol = "" ->
      "(" ^ String.concat " " (List.map debug_show_sexp1 xs) ^ ")"
  | SList (m, xs) ->
      "^" ^ m.symbol ^ " ("
      ^ String.concat " " (List.map debug_show_sexp1 xs)
      ^ ")"

let debug_show_cljexp nodes =
  let rec show_rec = function
    | Atom (m, x) when m.symbol = "" -> x
    | Atom (m, x) -> "^" ^ m.symbol ^ " " ^ x
    | RBList (m, xs) when m.symbol = "" ->
        "(" ^ String.concat " " (List.map show_rec xs) ^ ")"
    | RBList (m, xs) ->
        "^" ^ m.symbol ^ " (" ^ String.concat " " (List.map show_rec xs) ^ ")"
    | SBList (_, xs) -> "[" ^ String.concat " " (List.map show_rec xs) ^ "]"
    | CBList (_, xs) -> "{" ^ String.concat " " (List.map show_rec xs) ^ "}"
  in
  nodes |> List.map show_rec |> String.concat " "

let debug_show_sexp (nodes : sexp list) =
  let rec show_rec = function
    | SAtom (m, x) when m.symbol = "" -> x
    | SAtom (m, x) -> "^" ^ m.symbol ^ " " ^ x
    | SList (m, xs) when m.symbol = "" ->
        "(" ^ String.concat " " (List.map show_rec xs) ^ ")"
    | SList (m, xs) ->
        "^" ^ m.symbol ^ " (" ^ String.concat " " (List.map show_rec xs) ^ ")"
  in
  nodes |> List.map show_rec |> String.concat " " |> Printf.sprintf "[%s]"

let debug_show_sexp_for_error ?(show_pos = false) (nodes : sexp list) =
  let add_pos m =
    if show_pos then
      "|" ^ string_of_int m.line ^ ":" ^ string_of_int m.pos ^ "| "
    else ""
  in
  let rec show_rec = function
    | SAtom (m, x) when m.symbol = "" -> add_pos m ^ x
    | SAtom (m, x) -> "^" ^ m.symbol ^ " " ^ add_pos m ^ x
    | SList (m, xs) when m.symbol = "" ->
        "(" ^ String.concat " " (List.map show_rec xs) ^ ")"
    | SList (m, xs) ->
        "^" ^ m.symbol ^ " (" ^ String.concat " " (List.map show_rec xs) ^ ")"
  in
  nodes |> List.map show_rec |> String.concat " "

let log_sexp prefix node =
  prerr_endline @@ prefix ^ " " ^ debug_show_cljexp [ node ];
  node

let log_sexp2 prefix (node : sexp) =
  prerr_endline @@ prefix ^ " " ^ debug_show_sexp [ node ];
  node

let failnode prefix es =
  es
  |> List.map (fun x -> debug_show_cljexp [ x ])
  |> List.reduce_opt (Printf.sprintf "%s\n---\n%s")
  |> Option.value ~default:""
  |> Printf.sprintf "Can't parse:\n---------\n%s\n---------"
  |> prerr_endline;
  failwith ("Invalid node [" ^ prefix ^ "]")

let failsexp ?(show_pos = false) prefix (es : sexp list) =
  es
  |> List.map (fun x -> debug_show_sexp_for_error ~show_pos [ x ])
  |> List.reduce_opt (Printf.sprintf "%s\n---\n%s")
  |> Option.value ~default:""
  |> Printf.sprintf "Can't parse:\n---------\n%s\n---------"
  |> prerr_endline;
  failwith ("Invalid node [" ^ prefix ^ "]")

let rec show_sexp sexp =
  let format template xs =
    xs |> List.map show_sexp
    |> List.reduce_opt (Printf.sprintf "%s %s")
    |> Option.value ~default:"" |> Printf.sprintf template
  in
  match sexp with
  | Atom (_, x) -> x
  | RBList (_, xs) -> format "(%s)" xs
  | SBList (_, xs) -> format "[%s]" xs
  | CBList (_, xs) -> format "{%s}" xs

let rec show_sexp2 (sexp : sexp) =
  let format template xs =
    xs |> List.map show_sexp2
    |> List.reduce_opt (Printf.sprintf "%s %s")
    |> Option.value ~default:"" |> Printf.sprintf template
  in
  match sexp with SAtom (_, x) -> x | SList (_, xs) -> format "(%s)" xs

let try_log prefix (log : bool) (node : cljexp) =
  if log then print_endline @@ prefix ^ " " ^ debug_show_cljexp [ node ];
  node

let try_slog prefix (log : bool) (node : sexp) =
  if log then print_endline @@ prefix ^ " " ^ debug_show_sexp [ node ];
  node

module Functions = struct
  let rec sexp_to_string = function
    | SAtom (_, s) -> s
    | SList (_, xs) ->
        "(" ^ String.concat " " (List.map sexp_to_string xs) ^ ")"

  let rec sexp_to_string2 = function
    | SAtom (_, s) when String.starts_with ~prefix:":" s -> unpack_symbol s
    | SAtom (_, s) when String.starts_with ~prefix:"\"" s -> unpack_string s
    | SAtom (_, s) -> s
    | SList (_, xs) ->
        "(" ^ String.concat " " (List.map sexp_to_string2 xs) ^ ")"

  let rec debug_obj_to_string = function
    | OList (_, xs) ->
        "(" ^ String.concat " " (List.map debug_obj_to_string xs) ^ ")"
    | OVector (_, xs) ->
        "[" ^ String.concat " " (List.map debug_obj_to_string xs) ^ "]"
    | OMap (_, xs) ->
        "{"
        ^ String.concat " "
            (List.map
               (fun (k, v) ->
                 debug_obj_to_string k ^ ": " ^ debug_obj_to_string v)
               xs)
        ^ "}"
    | OString (_, s) -> "\"" ^ Scanf.unescaped s ^ "\""
    | OInt (_, i) -> string_of_int i ^ "i"
    | OFloat (_, f) -> string_of_float f ^ "f"
    | OBool (_, b) -> string_of_bool b ^ "b"
    | ONil _ -> "nil"
    | OLambda _ -> "lambda"
    | OQuote (_, n) -> "(quote " ^ debug_show_sexp1 n ^ ")"

  let rec obj_to_string = function
    | OList (_, xs) -> "(" ^ String.concat " " (List.map obj_to_string xs) ^ ")"
    | OVector (_, xs) ->
        "[" ^ String.concat " " (List.map obj_to_string xs) ^ "]"
    | OMap (_, xs) ->
        "{"
        ^ String.concat " "
            (List.map
               (fun (k, v) -> obj_to_string k ^ " " ^ obj_to_string v)
               xs)
        ^ "}"
    | OString (_, s) -> "\"" ^ s ^ "\""
    | OInt (_, i) -> string_of_int i
    | OFloat (_, f) -> string_of_float f
    | OBool (_, b) -> string_of_bool b
    | ONil _ -> "nil"
    | OLambda _ -> "lambda"
    | OQuote (_, n) -> "(quote " ^ debug_show_sexp [ n ] ^ ")"

  let failobj loc objs =
    failwith @@ loc ^ " - "
    ^ String.concat " " (List.map debug_obj_to_string objs)

  let obj_equal a b =
    match (a, b) with
    | OInt (_, a), OInt (_, b) -> a = b
    | OBool (_, a), OBool (_, b) -> a = b
    | OString (_, a), OString (_, b) -> a = b
    | ONil _, ONil _ -> true
    | OQuote (_, a), OQuote (_, b) -> a = b
    | a, b -> failwith @@ debug_obj_to_string a ^ " != " ^ debug_obj_to_string b
end

module SexpUtil = struct
  let butlast node =
    match node with
    | SList (_, SAtom (_, "do*") :: children) ->
        children |> List.rev |> List.tl |> List.rev
    | _ -> []

  let last = function
    | SList (_, SAtom (_, "do*") :: children) ->
        List.nth children (List.length children - 1)
    | node -> node
end

module OUtils = struct
  module F = Functions

  let failobj loc x = Printf.sprintf "%s %s" loc (F.obj_to_string x) |> failwith

  let rec obj_to_sexp = function
    (* *)
    | OInt (m, x) -> SAtom (m, string_of_int x)
    | OFloat (m, x) -> SAtom (m, string_of_float x)
    | OString (m, x) -> SAtom (m, "\"" ^ x ^ "\"")
    | OList (m, xs) -> SList (m, List.map obj_to_sexp xs)
    | OQuote (_, x) -> x
    | ONil m -> SAtom (m, "nil")
    | OVector (m, xs) ->
        SList (m, SAtom (m, "vector") :: List.map obj_to_sexp xs)
    | OMap (m, xs) ->
        let items =
          List.concat_map (fun (k, v) -> [ obj_to_sexp k; obj_to_sexp v ]) xs
        in
        SList (m, SAtom (m, "hash-map") :: items)
    | x -> failobj __LOC__ x

  let rec obj_to_string = function
    (* *)
    | OInt (_, x) -> string_of_int x
    | OString (_, x) -> "\"" ^ x ^ "\""
    | OList (_, xs) -> List.map obj_to_string xs |> String.concat ""
    | OQuote (_, SAtom (_, x)) -> x
    | ONil _ -> "nil"
    | OVector (_, xs) -> List.map obj_to_string xs |> String.concat ""
    | OBool (_, x) -> string_of_bool x
    | OQuote (_, m) -> "(quote" ^ debug_show_sexp_for_error [ m ] ^ ")"
    | x -> failobj __LOC__ x

  let rec debug_obj_to_string = function
    | OInt (_, x) -> string_of_int x
    | OFloat (_, x) -> string_of_float x
    | OString (_, x) -> "\"" ^ x ^ "\""
    | OList (_, xs) ->
        List.map debug_obj_to_string xs
        |> String.concat " " |> Printf.sprintf "(%s)"
    | OQuote (_, SAtom (_, x)) -> x
    | ONil _ -> "nil"
    | OVector (_, xs) ->
        List.map debug_obj_to_string xs
        |> String.concat " " |> Printf.sprintf "[%s]"
    | OBool (_, x) -> string_of_bool x
    | OQuote (_, m) -> "(quote '" ^ debug_show_sexp_for_error [ m ] ^ "')"
    | OMap _ -> failwith "OMap"
    | OLambda _ -> failwith "OLambda"
end

module NamespaceUtils = struct
  let get_ns_from_path _ path = "m" ^ string_of_int (String.hash path)

  let mangle_name (ns : string) (name : string) : string =
    let result =
      Printf.sprintf "G%i%s%i%s" (String.length ns) ns (String.length name) name
    in
    (* prerr_endline @@ "LOG[mangle_name] " ^ ns ^ " | " ^ name ^ " -> " ^ result; *)
    result

  let mangle_from_path root path name =
    let ns = get_ns_from_path root path in
    let result = mangle_name ns name in
    (* prerr_endline @@ "LOG[mangle_from_path] (" ^ ns ^ ") " ^ root ^ " | " ^ path
    ^ " | " ^ name ^ " -> " ^ result; *)
    result

  let unmangle_symbol x =
    if String.starts_with ~prefix:"G" x then
      let nstr =
        Str.string_match (Str.regexp "G[0-9]+") x 0 |> ignore;
        let s = Str.matched_string x in
        String.sub s 1 (String.length s - 1)
      in
      (* prerr_endline @@ "LOG: '" ^ x ^ "' -> '" ^ a ^ "'"; *)
      let l1 = int_of_string nstr in
      (* let l1 = String.get x 1 |> String.make 1 |> int_of_string in *)
      let ns = String.sub x (1 + String.length nstr) l1 in
      (* let ns = "|" ^ ns ^ "|" in *)
      let name =
        (* let n = String.length nstr in *)
        if not (Str.string_match (Str.regexp "[0-9]+") x (l1 + 3)) then
          failwith (x ^ "|" ^ string_of_int l1) |> ignore;
        let l2str = Str.matched_string x in
        (* let l2 = l2str |> int_of_string in *)
        let start = l1 + 3 + String.length l2str in
        (* String.sub x (l1 + 2 + n) (String.length x - l1 - 2 - n) *)
        String.sub x start (String.length x - start)
      in
      (* let name = "|" ^ name ^ "|" in *)
      (ns, name)
    else ("", x)

  let path_to_namespace name path =
    let path = unpack_string path in
    (* prerr_endline @@ "LOG: path=" ^ path; *)
    let path = Str.global_replace (Str.regexp "\\.\\./") "" path in
    (* prerr_endline @@ "LOG: path=" ^ path; *)
    let path = String.map (fun x -> if x = '/' then '.' else x) path in
    (* prerr_endline @@ "LOG: path=" ^ path; *)
    let path = mangle_name path name in
    path
end

let log_stage log_enabled title node =
  (if log_enabled then
     let padding = String.make (max 0 (30 - String.length title)) ' ' in
     prerr_endline @@ "* " ^ title ^ padding ^ " -> " ^ debug_show_sexp [ node ]
     ^ "\n");
  node

let get_dir filename =
  filename |> String.split_on_char '/' |> List.rev |> List.tl |> List.rev
  |> String.concat "/"
