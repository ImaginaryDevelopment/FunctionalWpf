namespace WpfTypes
open System
open System.Diagnostics
open System.Windows
open System.Windows.Controls

open Helpers

module WpfHelpers =
    open System.Windows
    open System.Windows.Media
    open System.Windows.Markup
    open System.ComponentModel
    open Helpers

    // guard against the outer 'T and inner 'T being different types
    type PropertyMetadataType<'T> = 
        | DefaultValue of 'T
        | Callback of PropertyChangedCallback
        | PMBoth of defaultValue:'T * PropertyChangedCallback
        | Unsafe of PropertyMetadata

    type FStaticDP<'TContainer,'TValue> (propName,propMetadata) =
        let dp =
            let pm =
                match propMetadata with
                | DefaultValue (x:'TValue) -> PropertyMetadata(box x)
                | Callback f -> PropertyMetadata f
                | PMBoth (x,f) -> PropertyMetadata(x,f)
                | Unsafe pm -> pm
            DependencyProperty.RegisterAttached(propName,typeof<'TValue>,typeof<'TContainer>, pm)
        let setValue (dObj:DependencyObject,v:'TValue) = 
            dumpt "setValue" propName |> ignore
            let r = dObj.SetValue(dp,box v)
            logt "setValue.finished" propName
            r

        member __.GetValue(dObj:DependencyObject) =
            logt "GetValue" propName
            let r = dObj.GetValue(dp) :?> 'TValue
            logt "GotValue" propName
            r
        member __.SetValue dObj v = setValue(dObj,v)
        member __.SetValue' dObj (v:obj) = setValue(dObj, v :?> 'TValue)
        member __.DP = dp
        
    let getXaml(o:obj) =
        try
            XamlWriter.Save(o)
            |> Some
        with ex ->
            if ex.Message.StartsWith("Cannot serialize a nested public type") |> not then
                logt "Failed getXaml ex" ex
            None

    let tryDumpXaml (o:obj) = (getXaml o) |> dumpt "xaml?"
        
    type Tree<'T> = 
        | Leaf of 'T*('T Tree list)
    module Tree =
        let rec map f (Leaf(leaf,children))=
                Leaf(f leaf, children |> List.map (map f))
        let rec length (Leaf(leaf,children)) = 
            1 + (children |> Seq.map length |> Seq.sum)
        let rec flatten (Leaf(leaf,children) as l) =
        
            let result = 
                seq{
                    yield leaf
                    yield! children |> Seq.collect flatten
                }
                |> List.ofSeq
            
            if children.Length > result.Length + 1 then
                printfn "Flatten was bad (expected length to be >%i, but was %i)" (children.Length) result.Length
            if result.Length <> length l then
                printfn "Flatten WAS bad"
            result
            |> Seq.ofList
        
                
    type Child =
        | Logical of obj
        | Visual of DependencyObject
        | Both of DependencyObject
        with 
            static member GetValue =
                function
                | Logical o -> o
                | Visual v 
                | Both v -> box v
    type ChildTree = Tree<Child>
    
    let unionAll<'T> (left:'T list) (right:obj list) fJoin =
        let minCount = if left.Length > 0 || right.Length > 0 then min left.Length right.Length |> max 1 else 0
        let result = 
            seq{
                yield! 
                    left
                    |> Seq.map(fun l ->
                        Some l, right |> Seq.tryFind (fJoin l)
                    )
                let rights = 
                    right
                    |> Seq.filter(fun r ->
                        left |> Seq.exists(fun l -> fJoin l r) |> not
                    )
                    |> Seq.map(fun r -> None, Some r)
                    |> List.ofSeq
                if right.Length > left.Length && right.Length < 1 then
                    printfn "bad right"
                
                yield! rights
                    
            }
            |> List.ofSeq
        //if debug then printfn "Left:%i,Right:%i,Result:%i" left.Length right.Length result.Length
        if result.Length < minCount then
            printfn "bad unionall"
        result
    let getName (x:obj) : string option= 
        match x with
        | :? DependencyObject as dObj ->
            
            dObj.GetValue FrameworkElement.NameProperty
            |> Option.ofObj
            |> Option.map (fun x -> x:?> string)
        | _ -> None
        
    let getVisualChildren (dObj:DependencyObject) : DependencyObject seq =
        let getVChild i = VisualTreeHelper.GetChild(dObj,i)
        seq{
            match System.Windows.Media.VisualTreeHelper.GetChildrenCount dObj with
            | 0 -> ()
            | x ->
                let items = [0..x - 1] |> Seq.map getVChild
                yield! items
        }
        
    let getItemsControlElements (ic:ItemsControl) =
        [0.. ic.Items.Count - 1]
        |> Seq.map ic.ItemContainerGenerator.ContainerFromIndex
        
    let mapChildTree = List.map(Tree.map(function | Visual v -> "Visual", v.GetType().Name | Logical l -> "Logical", l.GetType().Name | Both v -> "Both", v.GetType().Name))

    let rec walkChildren (x:obj) : ChildTree seq = // turn element into sequence of children
        seq{
            match x with
            | null -> ()
            | :? ItemsControl as ic -> // items control won't have items stuff until rendered =(
                let items = 
                    getItemsControlElements ic
                    |> Seq.map(fun item -> ChildTree.Leaf (Visual item, walkChildren item |> List.ofSeq))
                    |> List.ofSeq
                yield! items |> dumpCount "items control items"
            | :? DependencyObject as dObj -> 
                let vChildren = getVisualChildren dObj |> List.ofSeq
                let items = LogicalTreeHelper.GetChildren(dObj) |> Seq.cast<obj> |> List.ofSeq
                let minCount = 
                    if vChildren.Length > 0 && items.Length > 0 then min vChildren.Length items.Length else 0
                let mated = unionAll vChildren items (fun x y -> Object.ReferenceEquals(x,y)) |> List.ofSeq
                if mated.Length < minCount then
                    printfn "bad unionall afterall"
                let unionMapped = 
                    mated
                    // can't get 2 None, but what are we gonna do?
                    |> Seq.choose(
                        function
                        | Some dObj,None ->
                            
                            ChildTree.Leaf(Visual dObj, walkChildren dObj |> List.ofSeq)
                            |> Some
                        | Some dObj, Some _ ->
                            ChildTree.Leaf(Both dObj, walkChildren dObj |> List.ofSeq)
                            |> Some
                        | None, Some l ->
                            ChildTree.Leaf(Logical l, walkChildren l |> List.ofSeq)
                            |> Some
                        | None, None ->
                            printfn "None? this shouldn't have happened"
                            None
                    )
                    |> List.ofSeq
                if unionMapped.Length < minCount then
                    printfn "bad unionmap"
                
                yield! unionMapped
                    
                
            | :? String -> ()
            | x -> x.GetType().Name |> dumpt "no match, non dep object" |> ignore
        }

    let getChildren (parent:FrameworkElement) =
        walkChildren parent |> Seq.collect Tree.flatten |> Seq.map Child.GetValue

    let findByNameOrChildren (name:string) (parent:FrameworkElement) : obj option = 
        match parent.FindName name with
        | null ->
            let childs = getChildren parent
            let f : obj -> bool =
                getName >> (
                            function 
                            | Some (n:string) -> n = name 
                            | _ -> false
            )
            childs |> Seq.tryFind f
        | x -> Some x


open WpfHelpers

type PasswordBoxAssistant () = // for another method to do dep props in f# check http://jyliao.blogspot.com/2007/12/working-with-f-dependencyproperty-and.html
    static let fstaticDp (p, pm) =
        p |> dumpt "Creating fstatic dp" |> ignore
        let r = FStaticDP<PasswordBoxAssistant,_>(p,pm)
        p |> dumpt "Created fstatic dp" |> ignore
        r
    static let boundPassword = fstaticDp("BoundPassword", PMBoth(String.Empty, PropertyChangedCallback PasswordBoxAssistant.OnBoundPasswordChanged))
    static let bindPassword = fstaticDp("BindPassword", PMBoth(false, PropertyChangedCallback PasswordBoxAssistant.OnBindPasswordChanged ))
    static let updatingPassword = fstaticDp("UpdatingPassword", DefaultValue false)

    static let hpHandler = RoutedEventHandler(PasswordBoxAssistant.HandlePasswordChanged)

    static member BoundPasswordProperty = boundPassword.DP
    static member GetBoundPassword (dObj:DependencyObject) =
        dObj.GetValue(boundPassword.DP)
    static member SetBoundPassword (dObj:DependencyObject, value:obj) =
        dObj.SetValue(boundPassword.DP, value)
    static member BindPasswordProperty = bindPassword.DP
    static member GetBindPassword x = bindPassword.GetValue x
    // can't take out var names here, wpf freaks out
    // also the v:obj is required otherwise it will auto-upcast to obj to calls setValue' and leave v generic which wpf will also not find
    static member SetBindPassword dObj (v:obj) = v |> bindPassword.SetValue' dObj
    static member UpdatingPasswordProperty = updatingPassword.DP

    static member private OnBoundPasswordChanged(d:DependencyObject) (e:DependencyPropertyChangedEventArgs) = 
        log "OnBoundPasswordChanged"
        match d with
        | null -> ()
        | _ when not <| bindPassword.GetValue d -> ()
        | :? PasswordBox as pbox ->
            pbox.PasswordChanged.RemoveHandler hpHandler
            let newPassword = e.NewValue :?> string
            log <| sprintf "OnBoundPasswordChanged and we are changing it to %s" newPassword
            
            if updatingPassword.GetValue pbox |> not then
                pbox.Password <- newPassword
            pbox.PasswordChanged.AddHandler hpHandler
            ()
        | _ -> ()
        log "OnBoundPasswordChanged.finished!"

    static member private OnBindPasswordChanged dp e =
        log "OnBindPasswordChanged"
        match dp with
        | null -> ()
        | :? PasswordBox as pbox ->
            let wasBound = e.OldValue :?> bool
            let needToBind = e.NewValue :?> bool
            if wasBound then
                pbox.PasswordChanged.RemoveHandler hpHandler
            if needToBind then
                pbox.PasswordChanged.AddHandler hpHandler
        | _ -> ()
        log "OnBindPasswordChanged.finished"

    static member HandlePasswordChanged(sender:obj) (_:RoutedEventArgs) =
        log "HandlePasswordChanged"
        let pbox = sender :?> PasswordBox
        if updatingPassword.GetValue pbox |> not then
            updatingPassword.SetValue pbox true
            log <| sprintf "HandlePasswordChanged.from %A to %s" (pbox.GetValue(boundPassword.DP)) pbox.Password
            PasswordBoxAssistant.SetBoundPassword(pbox, pbox.Password)
            //pbox.SetValue(boundPassword.DP, )
            updatingPassword.SetValue pbox false
            log <| sprintf "HandlePasswordChanged.finished as %s, %s" pbox.Password (PasswordBoxAssistant.GetBoundPassword pbox :?> string)
        else log "HandlePasswordChanged called while running"
