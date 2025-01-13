module FileReader = struct
  type _ Effect.t += Load : string -> string Effect.t

  let read filename = Effect.perform (Load filename)

  let with_stub_scope (content : string) f arg =
    let open Effect.Deep in
    Effect.Deep.try_with f arg
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with Load _ -> Some (fun (k : (a, _) continuation) -> continue k content) | _ -> None);
      }

  let with_scope f arg =
    let open Effect.Deep in
    Effect.Deep.try_with f arg
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Load path -> Some (fun (k : (a, _) continuation) -> continue k In_channel.(with_open_bin path input_all))
            | _ -> None);
      }
end

let last xs = List.nth xs (List.length xs - 1)
let butlast xs = List.rev (List.tl (List.rev xs))
let loc (a, b, c, _) = Printf.sprintf "%s:%i:%i" a b c

type meta = { line : int; pos : int; symbol : string } [@@deriving show]

let unknown_location = { line = 0; pos = 0; symbol = "" }
let meta_empty = { line = 0; pos = 0; symbol = "" }

type sexp = SAtom of meta * string | SList of meta * sexp list [@@deriving show]

type cljexp =
  | Atom of meta * string
  | RBList of meta * cljexp list
  | SBList of meta * cljexp list
  | CBList of meta * cljexp list
[@@deriving show]

let unpack_string x = if String.starts_with ~prefix:"\"" x then String.sub x 1 (String.length x - 2) else x
let unpack_symbol x = String.sub x 1 (String.length x - 1)
let get_type meta = if meta.symbol = "" || meta.symbol = ":private" then "Object" else meta.symbol
let get_type_or_var meta = if meta.symbol = "" || meta.symbol = ":private" then "var" else meta.symbol

let change_meta m node =
  match node with
  | Atom (_, x) -> Atom (m, x)
  | RBList (_, xs) -> RBList (m, xs)
  | CBList (_, xs) -> CBList (m, xs)
  | SBList (_, xs) -> SBList (m, xs)

let change_smeta m (node : sexp) = match node with SAtom (_, x) -> SAtom (m, x) | SList (_, xs) -> SList (m, xs)

let get_symbol = function
  | Atom (m, _) -> m.symbol
  | RBList (m, _) -> m.symbol
  | SBList (m, _) -> m.symbol
  | CBList (m, _) -> m.symbol

let get_sexp_symbol = function SAtom (m, _) -> m.symbol | SList (m, _) -> m.symbol
let unwrap_do = function RBList (_, Atom (_, "do*") :: xs) -> xs | x -> [ x ]
let unwrap_sexp_do = function SList (_, SAtom (_, "do*") :: xs) -> xs | x -> [ x ]
let unwrap_single_do = function RBList (_, [ Atom (_, "do*"); xs ]) -> xs | x -> x

module StringMap = struct
  include Map.Make (String)

  let pp pp_value fmt map =
    let bindings = bindings map in
    Format.fprintf fmt "@[<v>{";
    bindings
    |> List.iter (fun (key, value) -> Format.fprintf fmt "@,%a -> %a;" Format.pp_print_string key pp_value value);
    Format.fprintf fmt "@,}@]"
end

type function_decl = { params : cljexp list; body : cljexp list } [@@deriving show]

type obj =
  | OVector of obj list
  | OList of obj list
  | OMap of (obj * obj) list
  | OString of string
  | OInt of int
  | OBool of bool
  | ONil
  | OLambda of (obj list -> obj)
  | OQuote of sexp
[@@deriving show]

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

let show_error_location filename m = Printf.sprintf "%s:%d:%d" filename m.line m.pos
let debug_show_scope ctx = StringMap.bindings ctx.scope |> List.map (fun (k, _) -> k) |> String.concat ", "
let debug_show_macro ctx = StringMap.bindings ctx.macros |> List.map (fun (k, _) -> k) |> String.concat ", "
let debug_show_imports ctx = StringMap.bindings ctx.imports |> List.map (fun (k, _) -> k) |> String.concat ", "

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
    Effect.Deep.try_with f ()
      {
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

  let rec split_into_pairs = function a :: b :: rest -> (a, b) :: split_into_pairs rest | _ -> []
  let reduce loc f xs = match xs with [] -> failwith loc | xs -> List.fold_left f (List.hd xs) (List.tl xs)
  let reduce_opt f xs = match xs with [] -> None | xs -> Some (reduce "" f xs)
end

let rec debug_show_cljexp1 = function
  | Atom (m, x) when m.symbol = "" -> x
  | Atom (m, x) -> "^" ^ m.symbol ^ " " ^ x
  | RBList (m, xs) when m.symbol = "" -> "(" ^ String.concat " " (List.map debug_show_cljexp1 xs) ^ ")"
  | RBList (m, xs) -> "^" ^ m.symbol ^ " (" ^ String.concat " " (List.map debug_show_cljexp1 xs) ^ ")"
  | SBList (_, xs) -> "[" ^ String.concat " " (List.map debug_show_cljexp1 xs) ^ "]"
  | CBList (_, xs) -> "{" ^ String.concat " " (List.map debug_show_cljexp1 xs) ^ "}"

let rec debug_show_sexp1 = function
  | SAtom (m, x) when m.symbol = "" -> x
  | SAtom (m, x) -> "^" ^ m.symbol ^ " " ^ x
  | SList (m, xs) when m.symbol = "" -> "(" ^ String.concat " " (List.map debug_show_sexp1 xs) ^ ")"
  | SList (m, xs) -> "^" ^ m.symbol ^ " (" ^ String.concat " " (List.map debug_show_sexp1 xs) ^ ")"
(* | SBList (_, xs) -> "[" ^ String.concat " " (List.map debug_show_cljexp1 xs) ^ "]"
  | CBList (_, xs) -> "{" ^ String.concat " " (List.map debug_show_cljexp1 xs) ^ "}" *)

let debug_show_cljexp nodes =
  let rec show_rec = function
    | Atom (m, x) when m.symbol = "" -> x
    | Atom (m, x) -> "^" ^ m.symbol ^ " " ^ x
    | RBList (m, xs) when m.symbol = "" -> "(" ^ String.concat " " (List.map show_rec xs) ^ ")"
    | RBList (m, xs) -> "^" ^ m.symbol ^ " (" ^ String.concat " " (List.map show_rec xs) ^ ")"
    | SBList (_, xs) -> "[" ^ String.concat " " (List.map show_rec xs) ^ "]"
    | CBList (_, xs) -> "{" ^ String.concat " " (List.map show_rec xs) ^ "}"
  in
  nodes |> List.map show_rec |> String.concat " "

let debug_show_sexp (nodes : sexp list) =
  let rec show_rec = function
    | SAtom (m, x) when m.symbol = "" -> x
    | SAtom (m, x) -> "^" ^ m.symbol ^ " " ^ x
    | SList (m, xs) when m.symbol = "" -> "(" ^ String.concat " " (List.map show_rec xs) ^ ")"
    | SList (m, xs) -> "^" ^ m.symbol ^ " (" ^ String.concat " " (List.map show_rec xs) ^ ")"
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

let failsexp prefix (es : sexp list) =
  es
  |> List.map (fun x -> debug_show_sexp [ x ])
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
