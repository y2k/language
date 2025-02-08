type code_compiled = Code_compiled of { value : string } [@@deriving yojson]
type result_received = Result_received of { result : string } [@@deriving yojson]
type _ Effect.t += Update : Yojson.Safe.t -> unit Effect.t

module ProxyServer = struct
  module S = Tiny_httpd

  let start port =
    let server = S.create () ~port in
    S.add_route_handler ~meth:`GET server
      S.Route.(exact_path "read" return)
      (fun _ ->
        Signal.get code_compiled_of_yojson ~timeout:300.0
        |> Option.map (fun (Code_compiled x) -> x.value)
        |> Option.to_result ~none:(503, "Timeout exceeded")
        |> S.Response.make_string);
    S.add_route_handler ~meth:`POST server
      S.Route.(exact_path "write" return)
      (fun req ->
        let result = req.S.Request.body in
        Effect.perform (Update (Result_received { result } |> result_received_to_yojson));
        S.Response.make_string (Ok "OK"));
    Printf.printf "PROXY listening on http://%s:%d\n%!" (S.addr server) (S.port server);
    match S.run server with Ok () -> () | Error e -> raise e
end

type new_code_received = New_code_received of { code : string } [@@deriving yojson]

module NreplServer = struct
  module B = Bencode

  let handle_client ic oc =
    let result = B.decode (`Channel ic) in
    let session = "3cea014e-78e1-473e-b486-8f3cab55432a" in
    (match Option.bind (B.dict_get result "op") B.as_string with
    | Some "clone" ->
        B.encode (`Channel oc)
          (B.Dict [ ("id", B.String "1"); ("new-session", B.String session); ("status", B.List [ B.String "done" ]) ])
    | Some "load-file" ->
        let code = B.dict_get result "file" |> Fun.flip Option.bind B.as_string |> Option.get in
        Effect.perform (Update (New_code_received { code } |> new_code_received_to_yojson));
        let result =
          Signal.get result_received_of_yojson ~timeout:2.0
          |> Option.map (fun (Result_received x) -> x.result)
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

  let start port =
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
        handle_client (in_channel_of_descr client_sock) (out_channel_of_descr client_sock)
      done
    with e ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string e);
      close sock
end

module Compiler = struct
  open Lib__
  open Common

  let compile = Backend_bytecode.main { no_lint = true; virtual_src = "" } false "user.clj" Preludes.bytecode

  let rec start () =
    let codes =
      Signal.get new_code_received_of_yojson ~timeout:30.0 |> Option.map (fun (New_code_received x) -> x.code)
    in
    match codes with
    | Some code ->
        let compiled_code = compile code in
        Effect.perform (Update (Code_compiled { value = compiled_code } |> code_compiled_to_yojson))
    | None -> start ()
end

module Logger = struct
  let decorate f arg =
    let open Effect.Deep in
    Effect.Deep.match_with f arg
      {
        retc = Fun.id;
        exnc = (fun e -> Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()));
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Update value ->
                Some
                  (fun (k : (a, _) continuation) ->
                    value |> Yojson.Safe.pretty_to_string |> print_endline;
                    continue k (Signal.dispatch value))
            | _ -> None);
      }
end

let start () =
  [
    Domain.spawn (fun _ -> Logger.decorate NreplServer.start 8080);
    Domain.spawn (fun _ -> Logger.decorate ProxyServer.start 8081);
    Domain.spawn (fun _ -> Logger.decorate Compiler.start ());
  ]
  |> List.iter Domain.join
