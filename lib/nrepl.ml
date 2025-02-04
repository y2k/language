module Context = struct
  type state = { raw_code : string option; code : string option; result : string option }
  type t = { state : state Atomic.t }

  let empty () = { state = Atomic.make { raw_code = None; code = None; result = None } }

  let rec update_state (context : t) (f : state -> state) =
    let old_state = Atomic.get context.state in
    let new_state = f old_state in
    if Atomic.compare_and_set context.state old_state new_state then () else update_state context f
end

module ProxyServer = struct
  module S = Tiny_httpd

  let rec read_code (context : Context.t) =
    let state = Atomic.get context.state in
    match state.code with
    | Some code -> code
    | None ->
        Unix.sleepf 0.5;
        read_code context

  let start_proxy_server (context : Context.t) port =
    let server = S.create () ~port in
    S.add_route_handler ~meth:`POST server
      S.Route.(exact_path "write" return)
      (fun req ->
        let result = req.S.Request.body in
        Context.update_state context (fun state -> { state with result = Some result });
        S.Response.make_string (Ok "OK"));
    S.add_route_handler ~meth:`GET server
      S.Route.(exact "read" @/ return)
      (fun _ ->
        (* print_endline @@ Format.asprintf "%a" S.Request.pp req; *)
        let code = read_code context in
        (* S.Response.make_string (Ok (Format.asprintf "echo:@ %a@." S.Request.pp req)) *)
        S.Response.make_string (Ok code));
    Printf.printf "PROXY listening on http://%s:%d\n%!" (S.addr server) (S.port server);
    match S.run server with Ok () -> () | Error e -> raise e
end

module NreplServer = struct
  module B = Bencode

  let handle_client (context : Context.t) ic oc =
    try
      let result = B.decode (`Channel ic) in
      print_endline @@ "DECODED: " ^ B.pretty_print result;
      let session = "3cea014e-78e1-473e-b486-8f3cab55432a" in
      (match Option.bind (B.dict_get result "op") B.as_string with
      | Some "clone" ->
          B.encode (`Channel oc)
            (B.Dict [ ("id", B.String "1"); ("new-session", B.String session); ("status", B.List [ B.String "done" ]) ])
      (* { "file": "(ns user) (+ 1 1)",
           "file-path": "/Users/igor/Projects/language/lib/nrepl.ml",
           "op": "load-file",
           "session": "db639f15-5253-4884-bb2c-cf709679270b7" } *)
      | Some "load-file" ->
          let code = B.dict_get result "file" |> Fun.flip Option.bind B.as_string |> Option.get in
          Context.update_state context (fun state -> { state with raw_code = Some code });
          B.encode (`Channel oc)
            (B.Dict [ ("session", B.String session); ("value", B.String "2"); ("status", B.List [ B.String "done" ]) ])
      | Some "close" ->
          B.encode (`Channel oc) (B.Dict [ ("session", B.String session); ("status", B.List [ B.String "done" ]) ])
      | Some x -> failwith @@ __LOC__ ^ " Unknown command: " ^ x |> ignore
      | None -> failwith "No op found" |> ignore);
      flush oc
    with End_of_file -> print_endline "Client disconnected"

  let start_nrepl_server context port =
    let open Unix in
    let addr = ADDR_INET (inet_addr_any, port) in
    let sock = socket PF_INET SOCK_STREAM 0 in
    try
      bind sock addr;
      listen sock 5;
      Printf.printf "NREPL listening on http://127.0.0.1:%d\n%!" port;
      while true do
        let client_sock, _ = accept sock in
        print_endline "Client connected";
        let ic = in_channel_of_descr client_sock in
        let oc = out_channel_of_descr client_sock in
        handle_client context ic oc
      done
    with e ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string e);
      close sock
end

module Compiler = struct
  open Common

  let compile = Backend_bytecode.main { no_lint = true; virtual_src = "" } false "user.clj" Preludes.bytecode

  let rec start_compaling (context : Context.t) =
    let start_state = Atomic.get context.state in
    match start_state.raw_code with
    | Some raw_code ->
        let code = compile raw_code in
        let end_state = { start_state with code = Some code; raw_code = None } in
        Atomic.compare_and_set context.state start_state end_state |> ignore;
        start_compaling context
    | None ->
        Unix.sleepf 0.5;
        start_compaling context
end

let start () =
  let context = Context.empty () in
  [
    Domain.spawn (fun _ -> ProxyServer.start_proxy_server context 8080);
    Domain.spawn (fun _ -> NreplServer.start_nrepl_server context 8081);
    Domain.spawn (fun _ -> Compiler.start_compaling context);
  ]
  |> List.iter Domain.join
