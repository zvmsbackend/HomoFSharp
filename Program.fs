open System.CommandLine
open System.Diagnostics
open PeterO.Numbers
open System

type Op = Add | Sub | Mul | Div | Push | Operand of value : ERational

let precedence = function
    | Add | Sub -> 1
    | Mul | Div -> 2
    | Operand(_) -> 3

let showOp = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"

let opToFunc = function
    | Add -> (+)
    | Sub -> (-)
    | Mul -> (*)
    | Div -> (/)

type PrecedenceExpr = Expr of content : string * precedence : int
                    | Operator of op : Op

let ten = ERational.FromInt32(10)

let compute expr =
    let rec loop expr computeStack =
        match expr, computeStack with
        | [], [Operand(peek)] -> peek
        | _, Operand(x) :: Operand(y) :: op :: computeStack ->
            loop expr (Operand((opToFunc op) x y) :: computeStack)
        | item :: expr, _ ->
            loop expr (item :: computeStack)
    loop expr []

let toInfix expr =
    let rec loop expr computeStack =
        match expr, computeStack with
        | [], [Expr(content, _)] -> content
        | _, (Expr(contentl, prl) :: Expr(contentr, prr) :: Operator(op) :: computeStack) ->
            let prm = precedence op
            let formatter = match prl < prm, prr < prm || ((op = Op.Sub || op = Op.Div) && (prr = prm)) with
                            | false, false -> "{0}{1}{2}"
                            | true, false -> "({0}){1}{2}"
                            | false, true -> "{0}{1}({2})"
                            | true, true -> "({0}){1}({2})"
            loop expr (Expr(String.Format(formatter, contentl, showOp op, contentr), prm) :: computeStack)
        | Operand(value) :: expr, computeStack ->
            loop expr (Expr(string (int value), 4) :: computeStack)
        | op :: expr, computeStack ->
            loop expr (Operator(op) :: computeStack)
    loop expr []

let proveAll source target =
    let rec loop source expr opCount =
        if List.isEmpty source && opCount = 1 then
            if compute expr = target then
                Seq.singleton expr
            else
                Seq.empty
        else
            let ret = match source with
                      | [] -> Seq.empty
                      | item :: source -> seq { yield! loop source (Operand(item) :: expr) (opCount + 1) }
            let ret = if opCount > 1 then
                          seq { 
                              for op in [Add; Sub; Mul; Div] do
                                  yield! loop source (op :: expr) (opCount - 1)
                              yield! ret }
                      else
                          ret
            match source, expr with
            | a :: source, Operand(b) :: expr -> seq { 
                                                     yield! loop source (Operand(b * ten + a) :: expr) opCount 
                                                     yield! ret }
            | _ -> ret
    loop source [] 0

let prove source = proveAll source >> Seq.tryHead

let getInts n =
    let rec loop n acc =
        if n > 0
        then loop (n / 10) (ERational.FromInt32(n % 10) :: acc)
        else acc
    loop n []

[<EntryPoint>]
let main args =
    let rootCommand = new RootCommand()
    let target = new Argument<int>("target")
    let source = new Argument<int>("source", fun () -> 114514)
    let digest = new Option<bool>([|"-d"; "--digest"|])
    rootCommand.AddArgument(target)
    rootCommand.AddArgument(source)
    rootCommand.AddOption(digest)
    rootCommand.SetHandler(Action<int, int, bool>(fun target source digest -> do
        let source = getInts source
        let args = (source, ERational.FromInt32(target))
        let stopwatch = new Stopwatch()
        stopwatch.Start()
        if digest then
            match prove <|| args with
            | Some(data) -> data
                            |> toInfix
                            |> printfn "%d = %s" target
            | None -> printfn "No result found"
            stopwatch.Stop()
        else
            let result = args 
                        ||> proveAll
                        |> Seq.map toInfix
                        |> Seq.distinct
                        |> Seq.toList
            stopwatch.Stop()
            let count = result   
                        |> Seq.map (fun expr ->
                           printfn "%d = %s" target expr
                           1)
                        |> Seq.sum
            printfn "%d results found" count
        printfn "Fixed within %d milliseconds." stopwatch.ElapsedMilliseconds), target, source, digest)
    rootCommand.InvokeAsync(args).Result
