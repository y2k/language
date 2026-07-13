open Ast

type _ Effect.t += Gensym : meta -> sexpr Effect.t

let counter = ref 0
let depth = ref 0
let gensym meta = Effect.perform (Gensym meta)
let gensym_string meta = match gensym meta with SAtom (_, name) -> name | SList _ -> failwith __LOC__

let make meta =
  incr counter;
  SAtom (meta, Printf.sprintf "G__%d" !counter)

let run f =
  if !depth > 0 then f ()
  else
    Fun.protect
      ~finally:(fun () -> decr depth)
      (fun () ->
        incr depth;
        Effect.Deep.match_with f ()
          {
            retc = Fun.id;
            exnc = (fun e -> Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()));
            effc =
              (fun (type a) (eff : a Effect.t) ->
                match eff with
                | Gensym meta -> Some (fun (k : (a, _) Effect.Deep.continuation) -> Effect.Deep.continue k (make meta))
                | _ -> None);
          })
