type meta = { line : int; pos : int; symbol : string } [@@deriving show]

let unknown_location = { line = 0; pos = 0; symbol = "" }

type cljexp =
  | Atom of meta * string
  | RBList of cljexp list
  | SBList of cljexp list
  | CBList of cljexp list
[@@deriving show]

let failnode prefix es =
  es |> List.map show_cljexp |> List.fold_left ( ^ ) ""
  |> Printf.sprintf "Can't parse:\n-------\n%s\n-------"
  |> prerr_endline;
  failwith ("Invalid node [" ^ prefix ^ "]")

let log_sexp prefix node =
  print_endline @@ prefix ^ " " ^ show_cljexp node;
  node

module StringMap = Map.Make (String)

type function_decl = { params : cljexp list; body : cljexp list }

type context = {
  filename : string;
  loc : meta;
  start_line : int;
  macros : cljexp StringMap.t;
  functions : function_decl StringMap.t;
}

let empty_context =
  {
    filename = "";
    loc = unknown_location;
    start_line = 0;
    macros = StringMap.empty;
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

  let reduce f xs =
    match xs with
    | [] -> failwith "[REDUCE] List is empty"
    | xs -> List.fold_left f (List.hd xs) (List.tl xs)

  let reduce_opt f xs = match xs with [] -> None | xs -> Some (reduce f xs)
end
