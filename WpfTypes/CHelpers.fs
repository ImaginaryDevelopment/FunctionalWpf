[<System.Runtime.CompilerServices.Extension>] 
module WpfTypes.CHelpers
open System.Runtime.CompilerServices

[<Extension>]
let IsSome<'T>(o: 'T option) = 
    Option.isSome o
[<Extension>]
let IsNone<'T>(o: 'T option) = 
    Option.isNone o

[<Extension>]
let getMaybe1Of2 =
    function
        | Choice1Of2 x -> Some x
        | _ -> None
[<Extension>]
let getMaybe2Of2 =
    function
        | Choice2Of2 x -> Some x
        | _ -> None
[<Extension>]
let getMaybe1Of3 =
    function
        | Choice1Of3 x -> Some x
        | _ -> None
[<Extension>]
let getMaybe2Of3 =
    function
        | Choice2Of3 x -> Some x
        | _ -> None
[<Extension>]
let getMaybe3Of3 =
    function
        | Choice3Of3 x -> Some x
        | _ -> None


//let GetChoice1Of2 =
//    function
//    | Choice1Of2 x -> Some x
//    | _ -> None
//let GetChoice2Of2 =
//    function
//    | Choice2Of2 x -> Some x
//    | _ -> None


