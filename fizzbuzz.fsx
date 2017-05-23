open System

let fizzbuzz (x) = 
    match (x % 3, x % 5) with
    | (0,0) -> "FizzBuzz"
    | (0,_) -> "Fizz"
    | (_,0) -> "Buzz"
    | (_,_) -> x.ToString() 


type FizzbuzzBuilder() = 
    member this.Yield(value) = fizzbuzz(value)
    member x.Delay(f) = f() |> Seq.singleton
    member x.Delay(f : unit -> string seq) = f()
    member x.Combine(l, r) = Seq.append (Seq.singleton l) (Seq.singleton r)
    member x.Combine(l, r) = Seq.append (Seq.singleton l) r
    member x.For(g, f) = Seq.map f g


                
let f = FizzbuzzBuilder()
let result = f { 
        yield 1
        yield 2
        for x = 3 to 50 do yield x 
}               