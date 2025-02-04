type msg = ..

module EventBus = struct
  module StringMap = Map.Make (String)

  type t = { topics : msg list StringMap.t Atomic.t; id : int Atomic.t }

  let make () = { topics = Atomic.make StringMap.empty; id = Atomic.make 1 }

  let rec dispatch (context : t) (msg : msg) : unit =
    let old_state = Atomic.get context.topics in
    let new_state = old_state |> StringMap.map (fun xs -> msg :: xs) in
    if Atomic.compare_and_set context.topics old_state new_state then () else dispatch context msg

  let gen_id context = Atomic.fetch_and_add context.id 1 |> Int.to_string

  let rec update (context : t) (f : _ -> _) =
    let old_state = Atomic.get context.topics in
    let new_state = f old_state in
    if Atomic.compare_and_set context.topics old_state new_state then old_state else update context f

  let sync_time_sec = 0.3

  let listen (context : t) (filter : msg -> 'r option) (listener : 'r list -> unit) : unit =
    let id = gen_id context in
    update context (StringMap.add id []) |> ignore;
    let rec loop () =
      update context (fun topics -> StringMap.add id [] topics)
      |> StringMap.find id |> List.filter_map filter |> listener;
      Unix.sleepf sync_time_sec;
      loop ()
    in
    loop ()

  let listen_once (context : t) (listener : msg -> 'r option) ~timeout : 'r option =
    let id = gen_id context in
    update context (StringMap.add id []) |> ignore;
    let rec loop time_left =
      if time_left <= 0.0 then None
      else
        let messages = update context (fun topics -> StringMap.add id [] topics) |> StringMap.find id in
        match List.find_map listener messages with
        | Some x -> Some x
        | None ->
            Unix.sleepf sync_time_sec;
            loop (time_left -. sync_time_sec)
    in
    let result = loop timeout in
    update context (StringMap.remove id) |> ignore;
    result
end

type msg += CodeCompiled of string | ResultReceived of string

module ProxyServer = struct
  module S = Tiny_httpd

  let start_proxy_server (context : EventBus.t) port =
    let server = S.create () ~port in
    S.add_route_handler ~meth:`GET server
      S.Route.(exact_path "read" return)
      (fun _ ->
        EventBus.listen_once context (function CodeCompiled code -> Some code | _ -> None) ~timeout:300.0
        |> Option.to_result ~none:(503, "Timeout exceeded")
        |> S.Response.make_string);
    S.add_route_handler ~meth:`POST server
      S.Route.(exact_path "write" return)
      (fun req ->
        let result = req.S.Request.body in
        EventBus.dispatch context (ResultReceived result);
        S.Response.make_string (Ok "OK"));
    Printf.printf "PROXY listening on http://%s:%d\n%!" (S.addr server) (S.port server);
    match S.run server with Ok () -> () | Error e -> raise e
end

type msg += NewCodeReceived of string

module NreplServer = struct
  module B = Bencode

  let handle_client (context : EventBus.t) ic oc =
    let result = B.decode (`Channel ic) in
    (* print_endline @@ "DECODED: " ^ B.pretty_print result; *)
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
        EventBus.dispatch context (NewCodeReceived code);
        let result =
          EventBus.listen_once context (function ResultReceived code -> Some code | _ -> None) ~timeout:2.0
          |> Option.value ~default:"<TIMEOUT_ERROR>"
        in
        B.encode (`Channel oc)
          (B.Dict [ ("session", B.String session); ("value", B.String result); ("status", B.List [ B.String "done" ]) ])
    | Some "close" ->
        B.encode (`Channel oc) (B.Dict [ ("session", B.String session); ("status", B.List [ B.String "done" ]) ])
    | Some x -> failwith @@ __LOC__ ^ " Unknown command: " ^ x |> ignore
    | None -> failwith "No op found" |> ignore);
    flush oc;
    print_endline "NREPL client disconnected"

  let start_nrepl_server context port =
    let open Unix in
    let addr = ADDR_INET (inet_addr_any, port) in
    let sock = socket PF_INET SOCK_STREAM 0 in
    try
      bind sock addr;
      listen sock 5;
      Printf.printf "NREPL listening on tpc://127.0.0.1:%d\n%!" port;
      while true do
        let client_sock, _ = accept sock in
        print_endline "NREPL client connected";
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

  let start_compaling (context : EventBus.t) =
    EventBus.listen context
      (function NewCodeReceived raw_code -> Some raw_code | _ -> None)
      (function
        | code :: _ ->
            let compiled_code = compile code in
            EventBus.dispatch context (CodeCompiled compiled_code)
        | _ -> ())
end

let start () =
  let context = EventBus.make () in
  [
    Domain.spawn (fun _ -> ProxyServer.start_proxy_server context 8080);
    Domain.spawn (fun _ -> NreplServer.start_nrepl_server context 8081);
    Domain.spawn (fun _ -> Compiler.start_compaling context);
    Domain.spawn (fun _ ->
        EventBus.listen context
          (fun x -> Some x)
          (fun messages ->
            let log =
              messages |> List.rev
              |> List.map (function
                   | CodeCompiled code -> `Assoc [ ("type", `String "CodeCompiled"); ("code", `String code) ]
                   | ResultReceived result -> `Assoc [ ("type", `String "ResultReceived"); ("result", `String result) ]
                   | NewCodeReceived code -> `Assoc [ ("type", `String "NewCodeReceived"); ("code", `String code) ]
                   | _ -> failwith "Unknown command")
            in
            if log <> [] then print_endline (Yojson.pretty_to_string (`List log))));
  ]
  |> List.iter Domain.join
