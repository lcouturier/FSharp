open System

let fibonacci = Seq.unfold(fun (x,y) -> Some(x , (x + y, x))) (1,0)




