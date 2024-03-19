module A = Angstrom
open Frontend

let rec compile_ (context : context) (node : cljexp) : context * string =
  let compile node = compile_ context node |> snd in
  let with_context node = (context, node) in
  match node with
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" |> with_context
  | Atom (_, x) -> x |> with_context
  | RBList [ Atom (_, "+"); a; b ] ->
      Printf.sprintf "(%s+%s)" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, "-"); a; b ] ->
      Printf.sprintf "(%s-%s)" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, "*"); a; b ] ->
      Printf.sprintf "(%s*%s)" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, "/"); a; b ] ->
      Printf.sprintf "(%s/%s)" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, ">"); a; b ] ->
      Printf.sprintf "(%s>%s)" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, "<"); a; b ] ->
      Printf.sprintf "(%s<%s)" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, ">="); a; b ] ->
      Printf.sprintf "(%s>=%s)" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, "<="); a; b ] ->
      Printf.sprintf "(%s<=%s)" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, "not"); x ] ->
      Printf.sprintf "!%s" (compile x) |> with_context
  | RBList [ Atom (_, "="); a; b ] ->
      Printf.sprintf "Objects.equals(%s,%s)" (compile a) (compile b)
      |> with_context
  | RBList (Atom (_, "vector") :: xs) ->
      let args =
        xs |> List.map compile
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      Printf.sprintf "List.of(%s)" args |> with_context
  | RBList [ Atom (_, "if"); c; a; b ] ->
      Printf.sprintf "(%s ? %s : %s)" (compile c) (compile a) (compile b)
      |> with_context
  | RBList (Atom (_, "let*") :: SBList vals :: body) ->
      let rec parse_vals nodes =
        match nodes with
        | Atom (_, val_name) :: val_body :: remain ->
            "Object " ^ val_name ^ "=" ^ compile val_body ^ ";"
            ^ parse_vals remain
        | [] -> ""
        | xs -> fail_node xs
      in
      let svals = parse_vals vals in
      let sbody =
        body |> List.map compile |> List.rev
        |> List.mapi (fun i x -> (if i = 0 then "return " ^ x else x) ^ ";")
        |> List.rev |> String.concat ""
      in
      Printf.sprintf "let(()->{%s%s});" svals sbody |> with_context
  | RBList
      [
        Atom (_, "def");
        Atom (fname_meta, fname);
        RBList (Atom (_, "fn*") :: SBList args :: body);
      ] ->
      let get_type am = if am.symbol = "" then "Object" else am.symbol in
      let sargs =
        args
        |> List.map (function
             | Atom (am, aname) -> Printf.sprintf "%s %s" (get_type am) aname
             | x -> fail_node [ x ])
        |> List.reduce_opt (Printf.sprintf "%s, %s")
        |> Option.value ~default:""
      in
      let rec loop_body = function
        | [] -> ""
        | [ x ] -> Printf.sprintf "return %s" (compile x)
        | x :: xs -> Printf.sprintf "%s;%s" (compile x) (loop_body xs)
      in
      let sbody = loop_body body in
      Printf.sprintf "public static %s %s(%s){%s}" (get_type fname_meta) fname
        sargs sbody
      |> with_context
  (* Придумать что делать с этой копипастой *)
  | RBList (Atom (_, "comment") :: _) -> "" |> with_context
  | RBList (Atom (_, "module") :: body) ->
      body |> List.map compile
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> with_context
  (* Interop method call *)
  | RBList (Atom (_, ".") :: target :: mname :: args) ->
      let sargs =
        match args with
        | [] -> ""
        | args ->
            args |> List.map compile
            |> List.reduce (Printf.sprintf "%s, %s")
            |> Printf.sprintf "%s"
      in
      Printf.sprintf "%s.%s(%s)" (compile target) (compile mname) sargs
      |> with_context
  (* Constructor *)
  | RBList (Atom (_, "new") :: Atom (_, cnst_name) :: args) ->
      (if List.length args = 0 then ""
       else args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s"))
      |> Printf.sprintf "new %s(%s)" cnst_name
      |> with_context
  (* Function call *)
  | RBList (head :: args) ->
      (let sargs =
         if List.length args = 0 then ""
         else args |> List.map compile |> List.reduce (Printf.sprintf "%s,%s")
       in
       String.map (function '/' -> '.' | x -> x) (compile head)
       ^ "(" ^ sargs ^ ")")
      |> with_context
  (* Default *)
  | x -> fail_node [ x ]

let main (filename : string) code =
  let prelude_macros = {|(defmacro not= [a b] (list 'not (list '= a b)))|} in
  let prefix_lines_count =
    String.fold_left
      (fun acc c -> if c = '\n' then acc + 1 else acc)
      1 prelude_macros
  in
  String.concat "\n" [ prelude_macros; code ]
  |> Frontend.parse_and_simplify prefix_lines_count filename
  |> (fun (ctx, exp) -> compile_ ctx exp)
  |> snd |> String.trim
