open System

type StringToIntBuilder() = 
    member this.Bind(v,f) = 
        let (parsed,value) = Int32.TryParse(v)
        match parsed,value with
        | false,_    -> "Failed"
        | true,value -> f(value)
    member this.Return(x) = sprintf "%i" x

type OptionBuilder() = 
    member this.Bind(value,f) = 
        match value with
        | Some(x) -> f(x)
        | _       -> None  
    member this.Return(x) = Some(x)

type RoundBuilder(digits : int) = 
    let round (x : float) = Math.Round(x,digits)

    member this.Bind(value : float, f) = f(round(value))
    member this.Return(x) = round(x)    

type Result = Success of float | DivByZero

type DefinedBuilder() =
    member this.Bind ((x : Result), (f : float -> Result)) =
        match x with
        | Success(x) -> f(x)
        | DivByZero -> DivByZero
    member this.Return (x : 'a) = x
    

let sendMessage (q, msg) = printfn "%A" msg
type MsmqMessage<'a> = MsmqMessage of string * 'a    


type PublishBuilder() = 
    member x.Bind(MsmqMessage(q,value) : MsmqMessage<_>,f) = 
        sendMessage(q,value)
        f()
    member x.Return(_) = true
    member x.For(items : seq<MsmqMessage<_>>, f) =
        for MsmqMessage(q,value) in items do
            sendMessage(q,value)     
            