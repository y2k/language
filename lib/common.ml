let last xs = List.nth xs (List.length xs - 1)
let butlast xs = List.rev (List.tl (List.rev xs))
let loc (a, b, c, _) = Printf.sprintf "%s:%i:%i" a b c

type meta = { line : int; pos : int; symbol : string } [@@deriving show]

let unknown_location = { line = 0; pos = 0; symbol = "" }

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

let get_symbol = function
  | Atom (m, _) -> m.symbol
  | RBList (m, _) -> m.symbol
  | SBList (m, _) -> m.symbol
  | CBList (m, _) -> m.symbol

let unwrap_do = function RBList (_, Atom (_, "do*") :: xs) -> xs | x -> [ x ]

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

type context = {
  log : bool;
  filename : string;
  loc : meta;
  start_line : int;
  macros : cljexp StringMap.t;
  scope : (cljexp * context ref) StringMap.t;
  prelude_scope : unit StringMap.t;
  interpreter : context -> cljexp -> context * cljexp;
  base_ns : string;
  imports : context StringMap.t;
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

let debug_show_cljexp nodes =
  let rec show_rec = function
    | Atom (m, x) when m.symbol = "" -> x
    | Atom (m, x) -> "^" ^ m.symbol ^ " " ^ x
    | RBList (m, xs) when m.symbol = "" -> "(" ^ String.concat " " (List.map show_rec xs) ^ ")"
    | RBList (m, xs) -> "^" ^ m.symbol ^ " (" ^ String.concat " " (List.map show_rec xs) ^ ")"
    | SBList (_, xs) -> "[" ^ String.concat " " (List.map show_rec xs) ^ "]"
    | CBList (_, xs) -> "{" ^ String.concat " " (List.map show_rec xs) ^ "}"
  in
  nodes |> List.map show_rec |> String.concat "\n"

let log_sexp prefix node =
  prerr_endline @@ prefix ^ " " ^ debug_show_cljexp [ node ];
  node

let failnode prefix es =
  es
  |> List.map (fun x -> debug_show_cljexp [ x ])
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

let try_log prefix (log : bool) (node : cljexp) =
  if log then print_endline @@ prefix ^ " " ^ debug_show_cljexp [ node ];
  node