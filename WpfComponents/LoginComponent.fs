namespace WpfComponents

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open WpfTypes

module LoginComponent =
    open System.Windows
    open System.Windows.Controls
    open WpfHelpers
    open FunctionalXaml
    open FunctionalParsing
    open System.Windows.Markup

    let private pboxName = "INeedCandy"

    let handlePbBinding parent =
        let bindIt (pb:PasswordBox) =
            let binding = System.Windows.Data.Binding()
            binding.Path <- PropertyPath("Password")
            binding.Mode <- Data.BindingMode.OneWayToSource
            let bp = WpfTypes.PasswordBoxAssistant.BoundPasswordProperty

            pb.SetBinding(bp, binding) |> ignore
            WpfTypes.PasswordBoxAssistant.SetBindPassword pb (box true)

        findByNameOrChildren pboxName parent
        |> function
        | Some (:? PasswordBox as pb) ->
            bindIt pb
            true
        | Some x ->
            getXaml x 
            |> failwithf "Found something else %A"
        | None -> 
            walkChildren parent
            |> Seq.collect Tree.flatten
            |> Seq.map Child.GetValue
            |> Seq.choose (function | :? PasswordBox as pb -> Some pb | _ -> None)
            |> Seq.tryHead
            |> function
                | Some pb ->
                    bindIt pb
                | None ->
                    printfn "Did not find it =("
            false

    let makeLoginWindowXaml usernameExpr pwdExpr =
        // Header
        let header = [ label [width 100] "User Name"
                       label [width 100] "Password"
                       
                        ] |> stackpanel [] Horizontal

        //// Row
        let row =   [   textbox [width 100] usernameExpr // 
                        passbox pboxName [width 100] pwdExpr // <@@ fun (x:LoginCredential) -> x.Password @@> 
                    ]
                    |> stackpanel [] Horizontal

        // Data Template
        //let sampleTemplate = datatemplate row

        // Final composition
        let sampleGrid = [ header
                           row
                           button "submit" ] |> stackpanel [width 250] Vertical
                                             |> border Blue

        // Main Window
        let w = window [width 400; height 200; rawAttribute defaultDeclarations] sampleGrid
        w
    type LoginCredential() =
        member val Username:string = null with get,set //{ mutable Username:string; mutable Password:string}
        member val Password:string = null with get,set

    let makeLoginWindow () =
        let xaml =
            makeLoginWindowXaml
                <@@ fun (x:LoginCredential) -> x.Username @@>
                <@@ fun (x:LoginCredential) -> x.Password @@> 
            |> parseWindow
        try
            let w = XamlReader.Parse xaml :?> Window
            let model = LoginCredential()
            w.DataContext <- model
            handlePbBinding w |> ignore<bool>
            w, xaml, Some (fun () -> model)
        with _ ->
            null, xaml, None

    let MakeLoginWindow () = 
        let w,x,f = makeLoginWindow ()
        w, x, (match f with | Some f -> Func<_>(f) | None -> null)


