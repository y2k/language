open System
open System.Threading
open Telegram.Bot
open Telegram.Bot.Types
open Telegram.Bot.Extensions.Polling
open LiteDB
open System.Net
open MetaLang
open System.Text.Json

let rec jsonToMap (doc: JsonElement) : obj =
    match doc.ValueKind with
    | JsonValueKind.Number -> doc.GetInt64() |> box
    | JsonValueKind.String -> doc.GetString() |> box
    | JsonValueKind.Object ->
        doc.EnumerateObject()
        |> Seq.map (fun x -> x.Name, box <| jsonToMap x.Value)
        |> Map.ofSeq
        |> box
    | e -> failwithf "Unsupported type %O" e

let invokeCommand (db: LiteDatabase) (bot: ITelegramBotClient) (update: Update) =
    async {
        let collection = db.GetCollection("main")
        let envCol = db.GetCollection("env")

        let text =
            update.Message
            |> Option.ofObj
            |> Option.map (fun x -> x.Text)
            |> Option.defaultValue ""

        match text.Split ' ' with
        | [| "/set_env"; name; key; value |] ->
            db.BeginTrans() |> ignore

            envCol.DeleteMany
                (fun doc ->
                    doc.["user"].AsInt64 = update.Message.From.Id
                    && doc.["name"].AsString = name
                    && doc.["key"].AsString = key)
            |> ignore

            envCol.Insert(
                BsonDocument(
                    dict [ "user", BsonValue.op_Implicit update.Message.From.Id
                           "name", BsonValue.op_Implicit name
                           "key", BsonValue.op_Implicit key
                           "value", BsonValue.op_Implicit value ]
                )
            )
            |> ignore

            db.Commit() |> ignore
        | [| "/add"; name; url |] ->
            db.BeginTrans() |> ignore

            collection.DeleteMany
                (fun doc ->
                    doc.["user"].AsInt64 = update.Message.From.Id
                    && doc.["name"].AsString = name)
            |> ignore

            collection.Insert(
                BsonDocument(
                    dict [ "user", BsonValue.op_Implicit update.Message.From.Id
                           "name", BsonValue.op_Implicit name
                           "url", BsonValue.op_Implicit url ]
                )
            )
            |> ignore

            db.Commit() |> ignore
        | [| "/ls" |] ->
            let codes =
                collection.Find(Query.EQ("user", BsonValue.op_Implicit update.Message.From.Id))

            let message =
                codes
                |> Seq.mapi
                    (fun i doc ->
                        let name = doc.["name"].AsString
                        let url = doc.["url"].AsString
                        sprintf "%s = %s" name url)
                |> Seq.fold (fun a x -> sprintf "%s\n%s" a x) "Scripts:"

            do!
                bot.SendTextMessageAsync(ChatId.op_Implicit update.Message.Chat.Id, message)
                |> Async.AwaitTask
                |> Async.Ignore
        | [| "/rm"; name |] ->
            db.BeginTrans() |> ignore

            collection.DeleteMany
                (fun doc ->
                    doc.["user"].AsInt64 = update.Message.From.Id
                    && doc.["name"].AsString = name)
            |> ignore

            db.Commit() |> ignore
        | _ -> ()
    }

let handleInline (db: LiteDatabase) (bot: ITelegramBotClient) (update: Update) =
    async {
        let collection = db.GetCollection("main")
        let envCol = db.GetCollection("env")

        let name =
            update.InlineQuery.Query.Split ' ' |> Seq.head

        let args =
            update.InlineQuery.Query.Split ' '
            |> Array.tail
            |> Array.map box
            |> Array.toList

        printfn "LOG : query = %O (%A)" name args

        let userId = update.InlineQuery.From.Id

        let url =
            collection.Find
                (fun doc ->
                    doc.["user"].AsInt64 = userId
                    && doc.["name"].AsString = name)
            |> Seq.tryHead
            |> Option.map (fun doc -> doc.["url"].AsString)

        let env =
            envCol.Find
                (fun doc ->
                    doc.["user"].AsInt64 = update.InlineQuery.From.Id
                    && doc.["name"].AsString = name)
            |> Seq.map (fun doc -> doc.["key"].AsString, box doc.["value"].AsString)
            |> Map.ofSeq

        match url with
        | None -> ()
        | Some url ->
            let! code = (new WebClient()).AsyncDownloadString(Uri(url))

            let response =
                LanguageParser.compile code
                |> MetaLang.mapToCoreLang
                |> Interpreter.run
                    (Map.ofList [ "str",
                                  (fun (args: (unit -> obj) list) ->
                                      args
                                      |> List.map
                                          (fun f ->
                                              match f () with
                                              | :? string as s -> s
                                              | :? bool as b -> b.ToString()
                                              | :? RSexp as x -> let (RSexp x) = x in x.Trim('"').ToString()
                                              | x -> failwithf "Can't parse '%O' (%O) to string" x (x.GetType()))
                                      |> List.fold (sprintf "%O%O") ""
                                      |> box)
                                  "get-in",
                                  (fun (args: (unit -> obj) list) ->
                                      let m: Map<string, obj> = args.[0] () |> unbox
                                      let path: obj list = args.[1] () |> unbox

                                      path
                                      |> List.fold
                                          (fun ma k' ->
                                              let m: Map<string, obj> = unbox ma

                                              let k =
                                                  match k' with
                                                  | :? RSexp as x -> let (RSexp x) = x in x
                                                  | x -> failwithf "Can't parse '%O' (%O) to string" x (x.GetType())

                                              m.[unbox k])
                                          (box m)
                                      |> box) ])
                    "main"
                    (box env :: args)

            match response with
            | :? list<obj> as xs ->
                if xs.[0] = box (RSexp "download-json") then
                    let m: Map<string, obj> = unbox xs.[1]
                    let urlRaw: string = unbox m.["url"]
                    let url = urlRaw.Trim '"'

                    let onSuccessFunName =
                        match unbox m.["on-success"] with
                        | Defn (name, _, _, _) -> name
                        | n -> failwithf "Unsupported callback type %O" n

                    printfn "LOG :: URL :: %O" url

                    let json =
                        (new WebClient()).DownloadString(Uri url)
                        |> fun data -> JsonSerializer.Deserialize<JsonElement>(data)
                        |> jsonToMap

                    let response =
                        LanguageParser.compile code
                        |> MetaLang.mapToCoreLang
                        |> Interpreter.run
                            (Map.ofList [ "str",
                                          (fun (args: (unit -> obj) list) ->
                                              args
                                              |> List.map
                                                  (fun f ->
                                                      match f () with
                                                      | :? string as s -> s
                                                      | :? bool as b -> b.ToString()
                                                      | :? RSexp as x -> let (RSexp x) = x in x.Trim('"').ToString()
                                                      | x -> failwithf "Can't parse '%O' (%O) to string" x (x.GetType()))
                                              |> List.fold (sprintf "%O%O") ""
                                              |> box)
                                          "get-in",
                                          (fun (args: (unit -> obj) list) ->
                                              let m: Map<string, obj> = args.[0] () |> unbox
                                              let path: obj list = args.[1] () |> unbox

                                              path
                                              |> List.fold
                                                  (fun ma k' ->
                                                      let m: Map<string, obj> = unbox ma

                                                      let k =
                                                          match k' with
                                                          | :? RSexp as x -> let (RSexp x) = x in x
                                                          | x ->
                                                              failwithf
                                                                  "Can't parse '%O' (%O) to string"
                                                                  x
                                                                  (x.GetType())

                                                      m.[unbox k])
                                                  (box m)
                                              |> box) ])
                            onSuccessFunName
                            [ json ]

                    do!
                        bot.AnswerInlineQueryAsync(
                            update.InlineQuery.Id,
                            [ InlineQueryResults.InlineQueryResultArticle(
                                  Guid().ToString(),
                                  "Completed",
                                  InlineQueryResults.InputTextMessageContent(response.ToString())
                              ) ]
                        )
                        |> Async.AwaitTask
                else
                    failwithf "Unsupported effect %O" xs.[0]
            | _ -> failwithf "Unsupported response %O" response
    }

[<EntryPoint>]
let main argv =
    use db = new LiteDatabase("main.db")
    let collection = db.GetCollection("main")

    let bot =
        TelegramBotClient(Environment.GetEnvironmentVariable "TELEGRAM_TOKEN")

    bot.StartReceiving(
        { new IUpdateHandler with
            member _.get_AllowedUpdates() = [||]

            member _.HandleError(_, err, _) =
                printfn "LOG :: %O" err
                Tasks.Task.CompletedTask

            member _.HandleUpdate(bot, update, _) =
                async {
                    try
                        if not <| isNull update.InlineQuery then
                            do! handleInline db bot update
                        else if not <| isNull update.Message then
                            do! invokeCommand db bot update
                    with
                    | e -> printfn "ERROR :: %O" e
                }
                |> Async.StartAsTask
                |> (fun x -> x :> Tasks.Task) }
    )
    |> ignore

    printfn "Started..."
    Thread.Sleep -1
    0
