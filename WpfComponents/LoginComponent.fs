namespace WpfComponents

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open WpfTypes
open WpfTypes.Helpers

module LoginComponent =
    open System.Windows
    open System.Windows.Controls
    open WpfHelpers
    open FunctionalXaml
    open FunctionalParsing
    open System.Windows.Markup
    module LoginComponentInternals =
        let pboxName = "IAmAPassword"
        let btnName = "IAmASubmitButton"
    open LoginComponentInternals

    let handleBinding<'T when 'T :> Control> name (f:'T -> bool) parent = 
        let withMaybe fNone (x:obj option) =
            match x with
            | Some (:? 'T as ctrl) ->
                Some ctrl
            | Some x ->
                getXaml x
                |> failwithf "Found something else %A"
            | None -> fNone()
        findByNameOrChildren name parent
        |> withMaybe (fun () ->
            walkChildren parent
                |> Seq.collect Tree.flatten
                |> Seq.map Child.GetValue
                |> Seq.choose Object.As<'T> (*(function | :? 'T as ctrl -> Some ctrl | _ -> None)*)
                |> Seq.tryHead
        )
        |> function
            | Some ctrl ->
                f ctrl
            | None -> false
    let handleDiaglogPositiveButtonBinding parent btnName (w:Window) = 
        let bindIt (b:Button) =
            printfn "Name is %s" b.Name
            b.Click.Add (fun _ ->
                w.DialogResult <- Nullable true
                w.Close())
            true
        handleBinding btnName bindIt parent

    // this would be better off with a converter so that the target can be string or SecureString
    let handlePbBinding parent bindingExpr =
        let bindIt (pb:PasswordBox) =
            printfn "Name is %s" pb.Name
            let binding = System.Windows.Data.Binding()
            let bindingName = getPropertyName bindingExpr
            binding.Path <- PropertyPath(bindingName)
            binding.Mode <- Data.BindingMode.OneWayToSource
            let bp = WpfTypes.PasswordBoxAssistant.BoundPasswordProperty

            pb.SetBinding(bp, binding) |> ignore
            WpfTypes.PasswordBoxAssistant.SetBindPassword pb (box true)
            true

        handleBinding pboxName bindIt parent

    let makeLoginWindowXaml usernameExpr =
        // Header
        let header = [ label [width 100] "User Name"
                       label [width 100] "Password"
                        ] |> stackpanel [] Horizontal

        //// Row
        let row =   [   textbox [width 100] usernameExpr
                        passbox pboxName [width 100]
                    ]
                    |> stackpanel [] Horizontal

        // Data Template
        //let sampleTemplate = datatemplate row

        // Final composition
        let sampleGrid = [  header
                            row
                            button {Content="submit";Name=btnName} 
                            ] 
                            |> stackpanel [width 250] Vertical
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
            handleDiaglogPositiveButtonBinding w btnName w |> ignore<bool>
            match handlePbBinding w pwdExpr with
            | true ->
                Choice1Of3(w, xaml, Some (fun () -> model))
            | false -> 
                Choice2Of3(w,xaml, Some (fun () -> model))
        with _ ->
            Choice3Of3 xaml

