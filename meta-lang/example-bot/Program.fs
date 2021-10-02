open System
open System.Threading
open Telegram.Bot
open Telegram.Bot.Types
open Telegram.Bot.Extensions.Polling
open LiteDB

[<EntryPoint>]
let main argv =
    use db = new LiteDatabase("main.db")
    let collection = db.GetCollection("main")

    let bot =
        TelegramBotClient(Environment.GetEnvironmentVariable "TELEGRAM_TOKEN")

    bot.OnInlineQuery
    |> Observable.add (fun x -> printfn "LOG : Inline : %O" x)

    bot.OnInlineResultChosen
    |> Observable.add (fun x -> printfn "LOG : InlineChosen : %O" x)

    bot.StartReceiving(
        { new IUpdateHandler with
            member _.get_AllowedUpdates() = [||]

            member _.HandleError(_, err, _) =
                printfn "LOG :: %O" err
                Tasks.Task.CompletedTask

            member _.HandleUpdate(bot, update, _) =
                async {
                    let text =
                        update.Message
                        |> Option.ofObj
                        |> Option.map (fun x -> x.Text)
                        |> Option.defaultValue ""

                    match text.Split(' ', 2) with
                    | [| "/add"; code |] ->
                        db.BeginTrans() |> ignore

                        collection.Insert(
                            BsonDocument(
                                dict [ "_id", BsonValue.op_Implicit (Guid.NewGuid())
                                       "user", BsonValue.op_Implicit update.Message.From.Id
                                       "code", BsonValue.op_Implicit code ]
                            )
                        )
                        |> ignore

                        db.Commit() |> ignore
                    | [| "/ls" |] ->
                        let codes =
                            collection.Find(Query.EQ("user", BsonValue.op_Implicit update.Message.From.Id))

                        let message =
                            codes
                            |> Seq.fold
                                (fun a doc ->
                                    let code = doc.["code"].AsString
                                    sprintf "%s- %s\n" a code)
                                "Scripts:\n"

                        do!
                            bot.SendTextMessageAsync(ChatId.op_Implicit update.Message.Chat.Id, message)
                            |> Async.AwaitTask
                            |> Async.Ignore
                    | _ -> ()
                }
                |> Async.StartAsTask
                |> (fun x -> x :> Tasks.Task) }
    )
    |> ignore

    printfn "Started..."
    Thread.Sleep -1
    0
