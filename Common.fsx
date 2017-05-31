
module Common

open System
open System.Diagnostics
open System.Collections.Generic
open System.IO

module Extensions = 
    type Option<'a> with
        member this.GetOrElse(f) = 
            if (this.IsSome) then this.Value else f()


module IO = 
    let rec getFiles path = 
        seq {
            yield! Directory.GetFiles(path,"*.*")    
            for dir in Directory.GetDirectories(path) do
                yield! getFiles(dir)
        }    


    type System.IO.Directory with
        static member GetFilesLazy path = getFiles(path)
        static member AsyncGetFiles(path : string, searchPattern : string) =
                let dele = new Func<string * string, string[]>(Directory.GetFiles)
                Async.FromBeginEnd((path, searchPattern),dele.BeginInvoke,dele.EndInvoke)

module Tools = 
    let measure f = 
        (fun args -> 
            let sw = Stopwatch()
            sw.Start()
            let result = f(args)            
            sw.Stop()
            (result, sw.Elapsed)
        )
        

    let memoize f = 
        let d = new Dictionary<_,_>()
        (fun args ->
            let (found,value) = d.TryGetValue(args)       
            if (found) then
                value
            else
                let result = f(args)
                d.Add(args, result)
                result
            )

    let rec randomGenerator (value : int) =    
        let r = Random()
        seq {
            yield value
            yield! randomGenerator(value + r.Next(100))
        }


    let generateRandomList = 
        let r = Random()
        Seq.unfold(fun x -> Some(x,(x + r.Next(100)))) (1)
