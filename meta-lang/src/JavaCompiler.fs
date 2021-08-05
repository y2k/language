module JavaCompiler

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

open MetaLang
module R = ExternalTypeResolver

let rec compile r program =
    // let env = ref env
    // let compile node = compile node !env
    let compile = compile r

    match program with
    | Cond bindNodes ->
        bindNodes
        |> List.mapi
            (fun i (c, b) ->
                if i = 0 then
                    sprintf "if (%s) { result = %s; }" (compile c) (compile b)
                else
                    sprintf "else if (%s) { result = %s; }" (compile c) (compile b))
        |> List.reduceString (sprintf "%s\n%s")
    | IsNull node ->
        let body = compile node
        sprintf "%s == null" body
    | Symbol s -> s
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
        if name = "intrinsic_new" then
            failwith "not implemented"
            // let clsName =
            //     match argNodes.[0] with
            //     | String x -> x
            //     | _ -> failwith "Invalid argument"

            // let resolve =
            //     R.resolveConstructorType r clsName (argNodes.Length - 1)

            // let args =
            //     argNodes
            //     |> List.skip 1
            //     |> List.map compile
            //     |> List.mapi (fun i p -> sprintf "(%s)%s" (resolve i) p)
            //     |> List.reduceString (sprintf "%s, %s")

            // sprintf "new %s(%s)" clsName args
        // elif name = "intrinsic_invoke_static" then
        //     let (clsName, methodName) =
        //         match argNodes.[0] with
        //         | String x ->
        //             match x.Split("/") with
        //             | [| a; b |] -> a, b
        //             | _ -> failwith "Invalid argument"
        //         | _ -> failwith "Invalid argument"

        //     let resolve =
        //         R.resolveStatic r clsName methodName (argNodes.Length - 1)

        //     let args =
        //         argNodes
        //         |> List.skip 1
        //         |> List.map compile
        //         |> List.mapi (fun i p -> sprintf "(%s)%s" (resolve i) p)
        //         |> List.reduceString (sprintf "%s, %s")

        //     sprintf "%s.%s(%s)" clsName methodName args
        // elif name = "intrinsic_invoke" then
        //     let (clsName, methodName) =
        //         match argNodes.[0] with
        //         | String x ->
        //             match x.Split("/") with
        //             | [| a; b |] -> a, b
        //             | _ -> failwith "Invalid argument"
        //         | _ -> failwith "Invalid argument"

        //     let instance = compile argNodes.[1]

        //     let resolve =
        //         R.resolve r clsName methodName (argNodes.Length - 2)

        //     let args =
        //         argNodes
        //         |> List.skip 2
        //         |> List.map compile
        //         |> List.mapi (fun i p -> sprintf "(%s)%s" (resolve i) p)
        //         |> List.reduceString (sprintf "%s, %s")

        //     sprintf "((%s)%s).%s(%s)" clsName instance methodName args
        else
            let args =
                argNodes
                |> List.map compile
                |> List.reduceString (sprintf "%s, %s")

            sprintf "((Function)%s).apply(new Object[] {%s})" name args
    | Def (name, node) ->
        let value = compile node

        sprintf
            """
public static final Object %s;
static {
    Object result;
    %s;
    %s = result;
}
"""
            name
            value
            name
    | Defn (name, args, nodes) ->
        // env
        // := Environment.register !env (Environment.Func(name, args))

        let ps =
            args
            |> List.mapi (fun i (a, _) -> sprintf "Object %s = args[%i];" a i)
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

        sprintf
            """
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
        |> sprintf
            """
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

@SuppressWarnings({"unchecked", "CastCanBeRemovedNarrowingVariableType", "ConstantConditions", "unused", "rawtypes", "SillyAssignment", "UnusedAssignment"})
class Module {
    private static final Function<Object[], Object> intrinsic_set = args -> {
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
