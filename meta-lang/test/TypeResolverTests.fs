module TypeResolverTests

open Xunit
open MetaLang

[<Fact>]
let test () =
    let env = ExternalTypeResolver.loadDefault ()
    let ctx = TypeResolver.defaultContext

    LanguageParser.compile "(module (defn foo [a] (foo 1)))"
    |> mapToCoreLang
    |> TypeResolver.resolve env ctx
