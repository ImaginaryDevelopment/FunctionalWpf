﻿namespace WpfComponents

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

    let handlePbBinding parent bindingExpr =
        let bindIt (pb:PasswordBox) =
            let binding = System.Windows.Data.Binding()
            let bindingName = getPropertyName bindingExpr
            binding.Path <- PropertyPath(bindingName)
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

    let makeLoginWindowXaml usernameExpr =
        // Header
        let header = [ label [width 100] "User Name"
                       label [width 100] "Password"
                       
                        ] |> stackpanel [] Horizontal

        //// Row
        let row =   [   textbox [width 100] usernameExpr
                        passbox pboxName [width 100] // <@@ fun (x:LoginCredential) -> x.Password @@> 
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
        let pwdExpr = <@@ fun (x:LoginCredential) -> x.Password @@>
        let xaml =
            makeLoginWindowXaml
                <@@ fun (x:LoginCredential) -> x.Username @@>
            |> parseWindow
        try
            let w = XamlReader.Parse xaml :?> Window
            let model = LoginCredential()
            w.DataContext <- model
            handlePbBinding w pwdExpr |> ignore<bool>
            Choice1Of2(w, xaml, Some (fun () -> model))
        with _ ->
            Choice2Of2 xaml

    let MakeLoginWindow () =
        match makeLoginWindow () with
        | Choice1Of2(w,x,f) ->
            Choice1Of2(w, x, (match f with | Some f -> Func<_>(f) | None -> null))
        | Choice2Of2 x -> Choice2Of2 x

