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
end

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

  let resolve_env_in_path path =
    let var = ".+\\$LY2K_PACKAGES_DIR" in
    match Sys.getenv_opt "LY2K_PACKAGES_DIR" with
    | Some value -> Str.global_replace (Str.regexp var) value path
    | None -> path

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
                    let path = resolve_env_in_path path in
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

let unwrap_sexp_do = function
  | SList (_, SAtom (_, "do*") :: xs) -> xs
  | x -> [ x ]

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
    | OMap (_, xs), OMap (_, ys) ->
        List.for_all2 (fun (a, b) (c, d) -> equal a c && equal b d) xs ys
    | a, b -> failobj __LOC__ [ a; b ]
end

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

let rec show_sexp2 (sexp : sexp) =
  let format template xs =
    xs |> List.map show_sexp2
    |> List.reduce_opt (Printf.sprintf "%s %s")
    |> Option.value ~default:"" |> Printf.sprintf template
  in
  match sexp with SAtom (_, x) -> x | SList (_, xs) -> format "(%s)" xs

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
  let rec obj_to_string = function
    | OInt (_, x) -> string_of_int x
    | OString (_, x) -> "\"" ^ x ^ "\""
    | OList (_, xs) -> List.map obj_to_string xs |> String.concat ""
    | OMap (_, xs) ->
        xs
        |> List.map (fun (k, v) ->
            Printf.sprintf "%s %s" (obj_to_string k) (obj_to_string v))
        |> String.concat " " |> Printf.sprintf "{%s}"
    | OQuote (_, SAtom (_, x)) -> x
    | ONil _ -> "nil"
    | OVector (_, xs) -> List.map obj_to_string xs |> String.concat ""
    | OBool (_, x) -> string_of_bool x
    | OQuote (_, m) -> "(quote" ^ debug_show_sexp_for_error [ m ] ^ ")"
    | OFloat _ -> failwith __LOC__
    | OLambda _ -> failwith __LOC__

  let failobj loc x = Printf.sprintf "%s %s" loc (obj_to_string x) |> failwith

  let rec obj_to_sexp = function
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
    result

  let mangle_from_path root path name =
    let ns = get_ns_from_path root path in
    let result = mangle_name ns name in
    result

  let path_to_namespace name path =
    let path = unpack_string path in
    let path = Str.global_replace (Str.regexp "\\.\\./") "" path in
    let path = String.map (fun x -> if x = '/' then '.' else x) path in
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
