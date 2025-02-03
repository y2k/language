module S = Tiny_httpd
module B = Bencode
open Unix

(* let start () =
  let server = S.create () in
  (* echo request *)
  S.add_route_handler server
    (* S.Route.(exact "/" @/ return) *)
    S.Route.(return)
    (fun req ->
      print_endline @@ Format.asprintf "%a" S.Request.pp req;
      S.Response.make_string (Ok (Format.asprintf "echo:@ %a@." S.Request.pp req)));
  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with Ok () -> () | Error e -> raise e *)

let handle_client (ic, oc) =
  (* let buffer = Bytes.create 1024 in *)
  try
    let result = B.decode (`Channel ic) in
    print_endline @@ "DECODED: " ^ B.pretty_print result;
    let session = "3cea014e-78e1-473e-b486-8f3cab55432a" in
    (match Option.bind (B.dict_get result "op") B.as_string with
    | Some "clone" ->
        B.encode (`Channel oc)
          (B.Dict [ ("id", B.String "1"); ("new-session", B.String session); ("status", B.List [ B.String "done" ]) ])
    | Some "load-file" ->
        B.encode (`Channel oc)
          (B.Dict [ ("session", B.String session); ("value", B.String "2"); ("status", B.List [ B.String "done" ]) ])
    | Some "close" ->
        B.encode (`Channel oc) (B.Dict [ ("session", B.String session); ("status", B.List [ B.String "done" ]) ])
    | Some x -> failwith @@ __LOC__ ^ " Unknown command: " ^ x |> ignore
    | None -> failwith "No op found" |> ignore);
    flush oc
  with End_of_file -> print_endline "Client disconnected"

let start_server port =
  let addr = ADDR_INET (inet_addr_any, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  try
    bind sock addr;
    listen sock 5;
    Printf.printf "Server listening on port %d\n%!" port;
    while true do
      let client_sock, _ = accept sock in
      print_endline "Client connected";
      let ic = in_channel_of_descr client_sock in
      let oc = out_channel_of_descr client_sock in
      ignore (Thread.create handle_client (ic, oc))
    done
  with e ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string e);
    close sock

let start () =
  let port = 8080 in
  start_server port
