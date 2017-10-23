module WpfTypes.CHelpers
    let GetChoice1Of2 =
        function
        | Choice1Of2 x -> Some x
        | _ -> None
    let GetChoice2Of2 =
        function
        | Choice2Of2 x -> Some x
        | _ -> None


