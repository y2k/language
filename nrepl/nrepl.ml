module NreplServer = struct
  module B = Bencode
  module C = Lib__.Common

  let send_to_server host code =
    let open Unix in
    let ic, oc = open_connection (Unix.ADDR_INET (Unix.inet_addr_of_string host, 18090)) in
    let len = String.length code in
    output_byte oc 0;
    output_byte oc 0;
    output_byte oc ((len lsr 8) land 0xff);
    output_byte oc (len land 0xff);
    output_string oc code;
    flush oc;
    (* shutdown (descr_of_out_channel oc) SHUTDOWN_SEND; *)
    In_channel.input_all ic

  let compile =
    let open Lib__ in
    Backend_bytecode.main { C.config_default with no_lint = true; no_deps = true } "user.clj" Preludes.bytecode

  let rec to_json = function
    | B.String x -> `String x
    | B.Dict xs -> `Assoc (List.map (fun (k, v) -> (k, to_json v)) xs)
    | B.Integer x -> `Int (Int64.to_int x)
    | B.List xs -> `List (List.map to_json xs)

  let handle_client host ic oc =
    let result = B.decode (`Channel ic) in
    print_endline @@ "INPUT: " ^ (to_json result |> Yojson.Safe.pretty_to_string);
    let session = "3cea014e-78e1-473e-b486-8f3cab55432a" in
    (match Option.bind (B.dict_get result "op") B.as_string with
    | Some "complete" ->
        B.encode (`Channel oc) (B.Dict [ ("session", B.String session); ("status", B.List [ B.String "done" ]) ])
    | Some "info" ->
        B.encode (`Channel oc) (B.Dict [ ("session", B.String session); ("status", B.List [ B.String "done" ]) ])
    | Some "clone" ->
        B.encode (`Channel oc)
          (B.Dict [ ("id", B.String "1"); ("new-session", B.String session); ("status", B.List [ B.String "done" ]) ])
    | Some "load-file" ->
        let code = B.dict_get result "file" |> Fun.flip Option.bind B.as_string |> Option.get in
        print_endline @@ "LOG:CODE: " ^ code;
        let code = code |> compile in
        print_endline @@ "LOG:COMPILED: '" ^ code ^ "'";
        let result = send_to_server host code in
        print_endline @@ "LOG:RESULT: '" ^ result ^ "'";
        B.encode (`Channel oc)
          (B.Dict [ ("session", B.String session); ("value", B.String result); ("status", B.List [ B.String "done" ]) ])
    | Some "close" ->
        B.encode (`Channel oc) (B.Dict [ ("session", B.String session); ("status", B.List [ B.String "done" ]) ])
    | Some x -> failwith @@ __LOC__ ^ " Unknown command: " ^ x |> ignore
    | None -> failwith "No op found" |> ignore);
    flush oc;
    print_endline "NREPL client disconnected"

  let start host port =
    prerr_endline @@ "NRepl started on port " ^ string_of_int port;
    let open Unix in
    let addr = ADDR_INET (inet_addr_any, port) in
    let sock = socket PF_INET SOCK_STREAM 0 in
    try
      bind sock addr;
      listen sock 5;
      while true do
        let client_sock, _ = accept sock in
        handle_client host (in_channel_of_descr client_sock) (out_channel_of_descr client_sock)
      done
    with e ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string e);
      close sock
end

let start client_host = NreplServer.start client_host 8080
