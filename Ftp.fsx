
module System.Ftp

open System
open System.Net
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Diagnostics


type Agent<'T> = MailboxProcessor<'T>

type FtpMessage = 
    | Put of ICredentials * string * Uri * AsyncReplyChannel<bool>
    | PutAsync of ICredentials * string * Uri 
    | Get of ICredentials * Uri * string * AsyncReplyChannel<bool>     
    | GetAsync of ICredentials * Uri * string     
    | Stop


type System.Net.WebRequest with
    member this.GetRequestAsync() = Async.AwaitTask(this.GetRequestStreamAsync())
    
type System.Net.WebClient with
    member this.DownloadAsync(uri : Uri) = Async.AwaitTask(Task.Run(fun _ -> this.DownloadData(uri)))        
    member this.DownloadAsync(uri : string) = this.DownloadAsync(Uri(uri))


let get (credentials : ICredentials,uri : Uri, filename : string) =     
    async {
        try                        
            use client = new WebClient()        
            client.Credentials <- credentials
            let! data = client.DownloadAsync(uri)
            use fs = new FileStream(filename, FileMode.Create)
            do! fs.AsyncWrite(data)                            
            return true
        with
        | _ as ex
            -> Trace.TraceError(ex.Message)
               return false            
    }

let createRequest (credentials : ICredentials)(uri : Uri) =     
        let request = WebRequest.Create(uri) :?> FtpWebRequest        
        request.Credentials <- credentials
        request.UseBinary <- true
        request.KeepAlive <- false
        request.UsePassive <- false
        request.Method <- WebRequestMethods.Ftp.UploadFile
        request

let put (credentials : ICredentials,filename : string, uri : Uri) = 
    async {                 
        Trace.TraceInformation("put => " + filename)
        try
            let request = createRequest(credentials)(uri)
            use fs = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
            let bytesToRead = int fs.Length
            let! data = fs.AsyncRead(bytesToRead)        
            request.ContentLength <- int64(bytesToRead)
            use! stream = request.GetRequestAsync()
            stream.Write(data, 0, data.Length);                       
            return true
        with
        | _ as ex
            -> Trace.TraceError(ex.Message)
               return false            
    }


type Agent(credentials : ICredentials) =     

    let agent = Agent.Start(fun inbox ->             
            let rec loop() =                                
                async { let! msg = inbox.Receive()
                        match msg with                                  
                        | Put(credentials,source,target,reply) ->                        
                            let! result = put(credentials,source,target)
                            reply.Reply(result)
                            return! loop()                                                      
                        | PutAsync(credentials,source,target) ->                        
                            let! result = put(credentials,source,target)
                            return! loop()     
                        | Get(credentials,source,target,reply) ->
                            let! result = get(credentials,source,target)
                            reply.Reply(result)
                            return! loop()                                                                                                                    
                        | GetAsync(credentials,source,target) ->
                            let! result = get(credentials,source,target)
                            return! loop()                                       
                        | Stop ->          
                            return ()                    
                }
            loop())

    new(user : string, pwd : string) = Agent(NetworkCredential(user, pwd)
    member this.Credentials = credentials
    member this.PutAsync(source, target) = agent.Post(PutAsync(this.Credentials,source,target))    
    member this.GetAsync(source, target) = agent.Post(GetAsync(this.Credentials,source,target))            
    member this.Put(source, target) = agent.PostAndReply(fun x -> Put(this.Credentials,source,target,x))    
    member this.Get(source, target) = agent.PostAndReply(fun x -> Get(this.Credentials,source,target,x))            
    member this.Stop() = agent.Post(Stop)    