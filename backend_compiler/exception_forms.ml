open Frontend

type t = { body : sexpr list; handler : (sexpr * sexpr list) option; cleanup : sexpr list option }

let error message = failwith ("try: " ^ message)
let nonempty label body = if body = [] then error (label ^ " must not be empty")

let parse items =
  let rec sections body handler cleanup = function
    | [] ->
        nonempty "body" body;
        if handler = None && cleanup = None then error "expected catch or finally";
        { body = List.rev body; handler; cleanup }
    | SList (_, _, SAtom (_, "catch") :: args) :: rest -> (
        if handler <> None || cleanup <> None then error "catch must occur once, before finally";
        match args with
        | (SAtom (_, name) as binding) :: forms when is_binding_symbol binding && not (String.contains name '/') ->
            nonempty "catch body" forms;
            sections body (Some (binding, forms)) cleanup rest
        | _ -> error "catch requires a simple symbol binding and a nonempty body")
    | SList (_, _, SAtom (_, "finally") :: forms) :: rest ->
        if cleanup <> None then error "finally must occur once";
        nonempty "finally body" forms;
        sections body handler (Some forms) rest
    | expr :: rest ->
        if handler <> None || cleanup <> None then error "body expression after a section";
        sections (expr :: body) handler cleanup rest
  in
  sections [] None None items

let throw_operand = function [ value ] -> value | _ -> failwith "throw: expected exactly one expression"
