let loc (a, b, c, _) = Printf.sprintf "%s:%i:%i" a b c

type meta = { line : int; pos : int; symbol : string } [@@deriving show]

let unknown_location = { line = 0; pos = 0; symbol = "" }

type cljexp =
  | Atom of meta * string
  | RBList of cljexp list
  | SBList of cljexp list
  | CBList of cljexp list
[@@deriving show]

let log_sexp prefix node =
  print_endline @@ prefix ^ " " ^ show_cljexp node;
  node

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

type context = {
  filename : string;
  loc : meta;
  start_line : int;
  macros : cljexp StringMap.t;
  scope : (cljexp * context) StringMap.t;
  interpreter : context -> cljexp -> context * cljexp;
}
[@@deriving show]

let show_error_location filename m =
  Printf.sprintf "%s:%d:%d" filename m.line m.pos

let empty_context =
  {
    filename = "";
    loc = unknown_location;
    start_line = 0;
    macros = StringMap.empty;
    scope = StringMap.empty;
    interpreter = (fun _ _ -> failwith __LOC__);
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

  let rec split_into_pairs = function
    | a :: b :: rest -> (a, b) :: split_into_pairs rest
    | _ -> []

  let reduce f xs =
    match xs with
    | [] -> failwith "[REDUCE] List is empty"
    | xs -> List.fold_left f (List.hd xs) (List.tl xs)

  let reduce_opt f xs = match xs with [] -> None | xs -> Some (reduce f xs)
end

let debug_show_cljexp nodes =
  let rec debug_show_cljexp_ = function
    | Atom (_, x) -> `String x
    | RBList xs -> `List (List.map debug_show_cljexp_ xs)
    | SBList xs -> `List (List.map debug_show_cljexp_ xs)
    | CBList xs -> `List (List.map debug_show_cljexp_ xs)
  in
  (match nodes with
  | [ n ] -> debug_show_cljexp_ n
  | xs -> `List (List.map debug_show_cljexp_ xs))
  |> Yojson.Safe.pretty_to_string

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
  | RBList xs -> format "(%s)" xs
  | SBList xs -> format "[%s]" xs
  | CBList xs -> format "{%s}" xs
