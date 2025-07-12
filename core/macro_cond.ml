open Common

let invoke compile m body =
  let rec loop = function
    | [ SAtom (_, ":else"); then_ ] -> then_
    | cond :: then_ :: body ->
        SList (m, [ SAtom (meta_empty, "if"); cond; then_; loop body ])
    | node -> failsexp __LOC__ node
  in
  loop body |> compile
