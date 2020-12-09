module JavaCompiler

open MetaLang

module List =
    let reduceString f xs =
        match xs with
        | [] -> ""
        | xs -> xs |> List.reduce f

    let rec mapCheck f xs =
        match xs with
        | [] -> []
        | x :: [] -> f true x :: []
        | x :: xs -> f false x :: mapCheck f xs

let rec compile program =
    // let env = ref env
    // let compile node = compile node !env

    match program with
    | Cond bindNodes ->
        bindNodes
        |> List.mapi
            (fun i (c, b) ->
                if i = 0
                then sprintf "if (%s) { result = %s; }" (compile c) (compile b)
                elif c = Bool true
                then sprintf "else { result = %s; }" (compile b)
                else sprintf "else if (%s) { result = %s; }" (compile c) (compile b))
        |> List.reduceString (sprintf "%s\n%s")
    | Bool b -> if b then "true" else "false"
    | IsNull node ->
        let body = compile node
        sprintf "%s == null" body
    | Symbol s -> s
    | String s -> sprintf "\"%s\"" s
    | Dic props ->
        props
        |> List.map (fun (k, v) -> sprintf "\"%s\", %s" k (compile v))
        |> List.reduceString (sprintf "%s,\n%s")
        |> sprintf "result = makeDictionary(\n%s)"
    | ReadDic (fname, node) ->
        let self = compile node
        sprintf """((Map<String, Object>) %s).get("%s")""" self fname
    | List (_) -> failwith "Not Implemented (List)"
    | Bind (binds, nodes) ->
        let localProps =
            binds
            |> List.map (fun (k, v) -> sprintf "Object %s = %s;" k (compile v))
            |> List.reduceString (sprintf "%s\n%s")

        let body =
            nodes
            |> List.map compile
            |> List.map (fun x -> x + ";")
            |> List.reduceString (sprintf "%s\n%s")

        sprintf "%s\n%s" localProps body
    | Call (name, argNodes) ->
        let args =
            argNodes
            |> List.map compile
            |> List.reduceString (sprintf "%s, %s")

        sprintf "((Function)%s).apply(new Object[] {%s})" name args
    | Def (name, node) ->
        let value = compile node
        sprintf """
public static final Object %s;
static {
    Object result;
    %s;
    %s = result;
}
"""
            name value name
    | Defn (name, args, nodes) ->
        // env
        // := Environment.register !env (Environment.Func(name, args))

        let ps =
            args
            |> List.mapi (fun i a -> sprintf "Object %s = args[%i];" a i)
            |> List.reduceString (sprintf "%s\n%s")

        // let backupEnv = !env

        // for a in args do
        //     env
        //     := Environment.register !env (Environment.Param a)

        let body =
            nodes
            |> List.map compile
            |> List.reduceString (sprintf "%s;%s")

        // env := backupEnv

        sprintf """
public static Function<Object[], Object> %s;
static {
    %s = args -> {
        %s
        Object result = null;
        %s;
        return result;
    };
}"""
            name
            name
            ps
            body
    | Module (_, nodes) ->
        nodes
        |> List.map compile
        |> List.reduce (sprintf "%s\n%s")
        |> sprintf """
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

@SuppressWarnings("unchecked")
class Module {
    private static Function<Object[], Object> intrinsic_set = args -> {
        Map<String, Object> dic = (Map<String, Object>) args[0];
        dic.put((String) args[1], args[2]);
        return null;
    };

    private static Map<String, Object> makeDictionary(Object... xs) {
        Map<String, Object> d = new HashMap<>();
        for (int i = 0; i < xs.length - 1; i += 2) d.put((String) xs[i], xs[i + 1]);
        return d;
    }
%s
}"""
