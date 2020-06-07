open Lwt
open Cohttp_lwt_unix

let server =
  Language.Library.App.example () |> ignore;

  let callback _conn _req body =
    ( body |> Cohttp_lwt.Body.to_string >|= fun _body ->
      Language.Library.App.get_json () )
    >>= fun body -> Server.respond_string ~status:`OK ~body ()
  in
  Server.create ~mode:(`TCP (`Port 8080)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
