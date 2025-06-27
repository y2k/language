open Lib__.Common

module OUtils = struct
  module F = Lib__.Backend_interpreter.Functions

  let failobj loc x = Printf.sprintf "%s %s" loc (F.obj_to_string x) |> failwith

  let rec obj_to_sexp = function
    (* *)
    | OInt (m, x) -> SAtom (m, string_of_int x)
    | OString (m, x) -> SAtom (m, "\"" ^ x ^ "\"")
    | OList (m, xs) -> SList (m, List.map obj_to_sexp xs)
    | OQuote (_, x) -> x
    | ONil m -> SAtom (m, "nil")
    | OVector (m, xs) ->
        SList (m, SAtom (m, "vector") :: List.map obj_to_sexp xs)
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
    | x -> failobj __LOC__ x
end

module NamespaceUtils = struct
  let get_ns_from_path _ path =
    (* prerr_endline @@ "LOG[get_ns_from_path] " ^ " | " ^ path; *)
    "m" ^ string_of_int (String.hash path)
  (* let path =
      String.sub path (String.length root)
        (String.length path - String.length root)
    in
    path
    |> (fun x ->
    if String.starts_with ~prefix:"/" x then String.sub x 1 (String.length x - 1)
    else x)
    |> Str.global_replace (Str.regexp "\\.clj") ""
    |> Str.global_replace (Str.regexp "/") "." *)

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
