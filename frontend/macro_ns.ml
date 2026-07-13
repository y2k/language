open Ast

let atom meta value = SAtom (meta, value)
let string_atom meta value = atom meta ("\"" ^ value ^ "\"")
let ns_error message = failwith ("ns: " ^ message)

let apply = function
  | SList (meta, Paren, SAtom (_, "ns") :: SAtom (_, namespace) :: clauses) ->
      let require_items = ref [] in
      let import_items = ref [] in
      let pair left right = SList (meta, Paren, [ string_atom meta left; string_atom meta right ]) in
      let add_clause = function
        | SList
            ( _,
              Paren,
              [ SAtom (_, ":require"); SList (_, Bracket, [ SAtom (_, required); SAtom (_, ":as"); SAtom (_, alias) ]) ]
            ) ->
            require_items := pair required alias :: !require_items
        | SList (_, Paren, SAtom (_, ":import") :: imports) ->
            let add_import = function
              | SList (_, Bracket, SAtom (_, package) :: classes) ->
                  List.iter
                    (function
                      | SAtom (_, name) -> import_items := pair name (package ^ "." ^ name) :: !import_items
                      | _ -> ns_error "import classes must be symbols")
                    classes
              | _ -> ns_error "expected (:require [namespace :as alias]) or (:import [package Class ...])"
            in
            if imports = [] then ns_error "expected (:require [namespace :as alias]) or (:import [package Class ...])";
            List.iter add_import imports
        | _ -> ns_error "expected (:require [namespace :as alias]) or (:import [package Class ...])"
      in
      List.iter add_clause clauses;
      Some
        (SList
           ( meta,
             Paren,
             [
               atom meta "compiler/ns";
               string_atom meta namespace;
               SList (meta, Paren, List.rev !require_items);
               SList (meta, Paren, List.rev !import_items);
             ] ))
  | SList (_, Paren, SAtom (_, "ns") :: _) -> ns_error "expected namespace symbol"
  | _ -> None
