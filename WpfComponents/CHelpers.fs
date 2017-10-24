module WpfComponents.CHelpers
open System

open LoginComponent

let getFuncFrom f = 
    Func<_>(f)
//let MakeLoginWindow () =
//    match makeLoginWindow () with
//    | Choice1Of3(w,x,f) ->
//        Choice1Of3(w, x, (match f with | Some f -> Func<_>(f) | None -> null))
//    | Choice2Of3(w,x,f) ->
//        Choice1Of3(w, x, (match f with | Some f -> Func<_>(f) | None -> null))
//    | Choice3Of3 x -> Choice3Of3 x
