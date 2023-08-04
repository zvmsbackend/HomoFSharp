open System.CommandLine
open System.Diagnostics
open PeterO.Numbers
open System

type Op = Add | Sub | Mul | Div | Push | Cons

let precedence = function
    | Op.Add | Op.Sub -> 1
    | Op.Mul | Op.Div -> 2
    | Op.Cons -> 3

let showOp = function
    | Op.Cons -> ""
    | Op.Add -> "+"
    | Op.Sub -> "-"
    | Op.Mul -> "*"
    | Op.Div -> "/"

type PrecedenceExpr = PrecedenceExpr of content : string * precedence : int

let compute source opStack =
    let ten = ERational.FromInt32(10)
    let rec loop opStack computeStack sourceStack =
        match opStack, computeStack, sourceStack with
        | [], peek :: _, _  -> peek
        | Op.Push :: opStack, _, item :: sourceStack ->
            loop opStack (item :: computeStack) sourceStack
        | op :: opStack, x :: y :: computeStack, _ ->
            loop opStack ((match op with
                            | Op.Add -> x + y
                            | Op.Sub -> x - y
                            | Op.Mul -> x * y
                            | Op.Div -> x / y
                            | Op.Cons -> x * ten + y) :: computeStack) sourceStack
    loop opStack [] source

let toInfix source opStack =
    let rec loop opStack computeStack sourceStack =
        match opStack, computeStack, sourceStack with
        | [], PrecedenceExpr(content, _) :: _, _ -> content
        | Op.Push :: opStack, _, item :: sourceStack -> 
            loop opStack (PrecedenceExpr((int item).ToString(), 4) :: computeStack) sourceStack
        | op :: opStack, PrecedenceExpr(content1, pr1) :: PrecedenceExpr(content2, pr2) :: computeStack, _ ->
            let currentPrecedence = precedence op
            let symbol = showOp op
            let formatter = match pr1 < currentPrecedence, pr2 < currentPrecedence || ((op = Op.Sub || op = Op.Div) && (pr2 = currentPrecedence)) with
                            | false, false -> "{0}{2}{1}"
                            | true, false -> "({0}){2}{1}"
                            | false, true -> "{0}{2}({1})"
                            | true, true -> "({0}){2}({1})"
            loop opStack (PrecedenceExpr(String.Format(formatter, content1, content2, symbol), currentPrecedence) :: computeStack) sourceStack
    loop opStack [] source

let proveAll source target =
    let len = (List.length source) * 2 - 1
    let rec loop a b opStack =
        seq {
            if a + b = len
            then 
                let opStack = List.rev opStack
                let result = compute source opStack
                if result = target then
                    yield opStack
            else
                if a <= len / 2 then
                    yield! loop (a + 1) b (Op.Push :: opStack)
                if a > b + 1 then
                    yield! [Op.Add; Op.Sub; Op.Mul; Op.Div] 
                            |> Seq.map (fun op -> loop a (b + 1) (op :: opStack))
                            |> Seq.concat
                let rec loop1 i =
                    seq {
                        if a + i < len / 2 && a > b - 2 then
                            yield! loop (a + 2 + i) (b + 1 + i) [
                                yield! Seq.replicate (i + 1) Op.Cons
                                yield! Seq.replicate (i + 2) Op.Push
                                yield! opStack]
                            yield! loop1 (i + 1)}
                yield! loop1 0}
    loop 0 0 []

let prove source = proveAll source >> Seq.tryHead

let getInts n =
    let rec loop n acc =
        if n > 0
        then loop (n / 10) (ERational.FromInt32(n % 10) :: acc)
        else List.rev acc
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
        if digest
        then 
            match prove <|| args with
            | Some(data) -> data
                            |> toInfix source
                            |> printfn "%d = %s" target
            | None -> printfn "No result found"
            stopwatch.Stop()
        else
            let result = args 
                        ||> proveAll
                        |> Seq.map (toInfix source)
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
