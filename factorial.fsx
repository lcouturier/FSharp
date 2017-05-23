open System
open System.Numerics
open System.Collections.Generic
open System.Diagnostics

#load "Common.fsx"
open Common.Tools

let rec factorial1 x = 
    match x with
    | 0 | 1 -> x
    | _ -> factorial1(x - 1) * x 

let rec factorialCached = memoize(fun x -> if (x <= 0) then 1 else x * factorialCached(x - 1))

let factorialCachedMemoize = measure(factorialCached)


let factorial2 value = 
    let rec util (x : BigInteger, acc : BigInteger) = 
        if (x = 1I) then acc else util(x - 1I, x * acc)
    util(value, 1I)


let factorial = Seq.unfold(fun (x,y) -> Some(x , (x * y, y + 1))) (1,1)


