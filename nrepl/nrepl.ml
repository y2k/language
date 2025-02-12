module NreplServer = struct
  module B = Bencode

  let send_to_server host code =
    let open Unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    let server_addr = ADDR_INET (inet_addr_of_string host, 8080) in
    let bytes = Bytes.of_string code in
    connect sock server_addr;
    write sock bytes 0 (Bytes.length bytes) |> ignore;
    let result = Bytes.make 1024 '0' in
    let len = read sock result 0 (Bytes.length result) in
    let result = Bytes.sub_string result 0 len in
    close sock;
    result

  let compile =
    let open Lib__ in
    Backend_bytecode.main { no_lint = true; virtual_src = "" } false "user.clj" Preludes.bytecode

  let handle_client host ic oc =
    let result = B.decode (`Channel ic) in
    let session = "3cea014e-78e1-473e-b486-8f3cab55432a" in
    (match Option.bind (B.dict_get result "op") B.as_string with
    | Some "clone" ->
        B.encode (`Channel oc)
          (B.Dict [ ("id", B.String "1"); ("new-session", B.String session); ("status", B.List [ B.String "done" ]) ])
    | Some "load-file" ->
        let code = B.dict_get result "file" |> Fun.flip Option.bind B.as_string |> Option.get |> compile in
        let result = send_to_server host code in
        B.encode (`Channel oc)
          (B.Dict [ ("session", B.String session); ("value", B.String result); ("status", B.List [ B.String "done" ]) ])
    | Some "close" ->
        B.encode (`Channel oc) (B.Dict [ ("session", B.String session); ("status", B.List [ B.String "done" ]) ])
    | Some x -> failwith @@ __LOC__ ^ " Unknown command: " ^ x |> ignore
    | None -> failwith "No op found" |> ignore);
    flush oc;
    print_endline "NREPL client disconnected"

  let start host port =
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
        handle_client host (in_channel_of_descr client_sock) (out_channel_of_descr client_sock)
      done
    with e ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string e);
      close sock
end

let start client_host = NreplServer.start client_host 8080
