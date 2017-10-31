module WpfTypes.Helpers
open System
open System.Diagnostics

module Map =
    let keys<'T,'TValue when 'T:comparison> (x:Map<'T,'TValue>) : 'T seq = 
        x 
        |> Seq.map (|KeyValue|)
        |> Seq.map fst

let delimit d (items: string seq) =
    let items' = items |> Array.ofSeq
    String.Join(separator=d,values=items')
type System.Object with
    static member As<'T> (x:obj) = 
        match x with
        | :? 'T as t -> Some t
        | _ -> None

type IDumper =
    abstract member Dump: description: string * value:'T -> unit
    abstract member Dump: value:'T -> unit

let mutable logger : IDumper option = None

let logt title (x:obj) =
    logger
    |> Option.iter(fun f -> f.Dump(title,x))

let log (value:string) =
    logger
    |> Option.iter(fun f -> f.Dump(null,value))

let mutable dumper:IDumper option = None
let dumpt t x =
    match dumper with
    | Some d -> d.Dump(t,x)
    | None -> ()
    x
let dumpCount t (x : 'T list) = x.Length |> dumpt t |> ignore<int>; x

let defaultDumper =
    //let mutable runBroadcasts = false
    let broadcast x =
        let tryIt f =
            try
                f()
            with _ -> ()

        tryIt (fun () -> printfn "%s" x)
        tryIt (fun () -> Trace.WriteLine x)
        tryIt (fun () -> System.Diagnostics.Debugger.Log(1,"Wee?", x))
    { new IDumper with
        member __.Dump(descr,value) = 
            broadcast <| sprintf "%s:%A" descr value
        member __.Dump(value) =
            broadcast <| sprintf "%A" value
    }
logger <- Some defaultDumper
