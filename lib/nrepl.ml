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

(* d11:new-session36:db639f15-5253-4884-bb2c-cf709679270b7:session36:8be2f58f-642a-4e2a-8c62-cb93328eb5b46:statusl4:doneee *)

let handle_client (ic, oc) =
  let buffer = Bytes.create 1024 in
  try
    while true do
      (* let line = input_byte ic in *)
      (* let line = input_line ic in *)
      let n = input ic buffer 0 (Bytes.length buffer) in
      let line = Bytes.sub_string buffer 0 n in
      (* print_endline line; *)
      let result = B.decode (`String line) in
      (* *)
      print_endline @@ "RESULT: " ^ (B.dict_get result "op" |> Option.get |> B.as_string |> Option.get);
      Printf.printf "Client: %s\n%!" line;
      flush_all ();
      (* output_string oc (line ^ "\n"); *)
      output_string oc ({|{:id "1", :new-session "abcd1234", :status ["done"]}|} ^ "\n");
      flush oc
    done
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
