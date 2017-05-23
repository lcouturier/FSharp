open System


type AggregateBuilder(separator) = 
    member x.Yield(v) = v.ToString()
    member x.Delay(f) = f()
    member x.Delay(f : unit -> string seq) = f()
    member x.For(g,f) = g |> Seq.map f |> Seq.reduce (fun l r -> x.Combine(l, r))
    member x.Combine(l, r) = String.Concat(l,separator, r)


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
            
let sendAllMessages() = 
    let p = PublishBuilder()
    p {                         
         do! MsmqMessage("www.google.fr", "Message 1")
         do! MsmqMessage("www.google.fr", "Message 2")
         do! MsmqMessage("www.google.fr", "Message 3")
    }
            

let values =
    let b = AggregateBuilder("; ")
    b {
        yield "Début";        
        for x = 1 to 9 do yield x
        yield "Fin";
    }            

type RetryBuilder(times) = 
    let rec retry (f, count : int) =         
        try 
            if count < times then Some(f()) else None                
        with 
        | ex -> 
            printfn "%A" ex.Message
            retry(f, count - 1)

    member this.Bind(value,_) = retry(value(),times)                  
    member this.Return(_) = None       