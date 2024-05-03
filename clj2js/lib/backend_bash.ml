module A = Angstrom
open Frontend

let rec compile_ (context : context) (node : cljexp) : context * string =
  let compile node = compile_ context node |> snd in
  let withContext node = (context, node) in
  (* expand_core_macro node |> show_cljexp |> print_endline; *)
  match node with
  | Atom (_, x) when String.uppercase_ascii x = x ->
      Printf.sprintf {|"${%s}"|} x |> withContext
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      "-" ^ String.sub x 1 (String.length x - 1) |> withContext
  | Atom (_, x) when String.starts_with ~prefix:"\"" x -> x |> withContext
  | Atom (_, x) -> x |> withContext
  (* ========================== *)
  | RBList (Atom (_, "println") :: args) ->
      let sargs =
        args |> List.map compile |> List.reduce (Printf.sprintf "%s %s")
      in
      Printf.sprintf "echo %s" sargs |> withContext
  | RBList (Atom (_, "defn") :: Atom (_, fname) :: SBList _args :: body) ->
      let sbody =
        body |> List.map compile |> List.reduce (Printf.sprintf "%s\n%s")
      in
      Printf.sprintf "\nfunction %s() {\n%s\n}" fname sbody |> withContext
  | RBList (Atom (_, "do") :: body) ->
      body |> List.map compile
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> withContext
  | RBList (Atom (_, "str") :: args) ->
      args
      |> List.map (function
           | Atom (_, x) when String.starts_with ~prefix:"\"" x ->
               String.sub x 1 (String.length x - 2)
           (* | x -> Printf.sprintf "${%s}" (compile x) *)
           | x -> Printf.sprintf "%s" (compile x))
      |> List.reduce (Printf.sprintf "%s%s")
      |> Printf.sprintf {|%s|} |> withContext
  | RBList ([ Atom (_, "if"); _; _ ] as prefix) ->
      compile (RBList (prefix @ [ Atom (unknown_location, "") ])) |> withContext
  | RBList [ Atom (_, "if"); condition; then_; else_ ] ->
      let selse =
        match else_ with
        | Atom (_, "") -> ""
        | else_ -> Printf.sprintf "else\n%s\n" (compile else_)
      in
      let scondition =
        match condition with
        | RBList [ Atom (_, "not"); RBList [ Atom (_, "empty?"); arg ] ] ->
            Printf.sprintf {|[[ -n "$%s" ]]|} (compile arg)
        | condition -> compile condition
      in
      Printf.sprintf "if %s; then\n%s\n%sfi" scondition (compile then_) selse
      |> withContext
  | RBList (Atom (_, "let") :: SBList vals :: body) ->
      let rec parse_vals nodes =
        match nodes with
        | Atom (_, val_name) :: val_body :: remain ->
            Printf.sprintf "local %s=%s\n%s" val_name (compile val_body)
              (parse_vals remain)
        | [] -> ""
        | xs -> failnode __LOC__ xs
      in
      let svals = parse_vals vals in
      let sbody =
        body |> List.map compile |> List.rev
        |> List.mapi (fun i x -> if i = 0 then "" ^ x else x)
        |> List.rev
        |> List.reduce (Printf.sprintf "%s; %s")
      in
      "" ^ svals ^ sbody ^ "" |> withContext
  (* Functions or Macro calls *)
  | RBList (Atom (_, fname) :: args) ->
      (let sargs =
         if List.length args = 0 then ""
         else args |> List.map compile |> List.reduce (Printf.sprintf "%s %s")
       in
       fname ^ " " ^ sargs)
      |> withContext
  | n -> failnode __LOC__ [ n ]

let main (filename : string) code =
  Frontend.parse_and_simplify StringMap.empty 0 filename code
  |> (fun (ctx, exp) -> compile_ ctx exp)
  |> snd |> String.trim
