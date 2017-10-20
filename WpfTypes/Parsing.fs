namespace WpfTypes

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

// derived from http://fsharpcode.blogspot.it/2011/07/functional-wpf-part-five-simple-example.html
module FunctionalXaml =

    type Color = Red | Green | Blue
    type Orientation = Horizontal | Vertical

    type Attribute = 
        | Width of int
        | Height of int
        | RawAttribute of string
        | NsDeclaration of string*string

    type DataTemplate = DataTemplate of FunFrameworkElement
    and FunFrameworkElement
        = Label of string * Attribute list
        | Button of String
        | TextBox of Attribute list * Expr
        | PasswordBox of name:string*Attribute list * Expr
        | StackPanel of FunFrameworkElement list * Attribute list * Orientation
        | Border of Color * FunFrameworkElement
        | ItemsControl of DataTemplate
    type FunWindow = Window of FunFrameworkElement * Attribute list
    type Application = Application of FunWindow * Map<string,string>

    let width = Width
    let height = Height
    let stackpanel attrs orient xs = StackPanel(xs,attrs,orient)
    let border c x = Border(c,x)
    let label attrs s = Label(s,attrs)
    let button = Button
    let textbox attrs b = TextBox(attrs,b)
    let rawAttribute x = RawAttribute x
    let itemscontrol = ItemsControl
    let datatemplate = DataTemplate
    let application = Application
    let window attrs c = Window(c, attrs)
    let passbox name attrs b = PasswordBox(name,attrs,b)

open FunctionalXaml

module FunctionalParsing =
    open Helpers

    let mutable logger : (string*obj -> unit) option = None
    let log title (x:obj) =
        logger
        |> Option.iter(fun f -> f (title,x))

    let parseColor =
        function
        | Red -> "Red"
        | Green -> "Green"
        | Blue -> "Blue"

    let parseOrientation o =
        match o with
        | Horizontal -> "Horizontal"
        | Vertical -> "Vertical"

    let parseAttribute attr =
        match attr with
        | Width x -> sprintf @"Width = ""%i""" x
        | Height x -> sprintf @"Height = ""%i""" x
        | RawAttribute x -> x
        | NsDeclaration (k,v) -> 
            match k with 
            | null -> 
                sprintf "xmlns=\"%s\"" v
            | k -> sprintf "xmlns:%s=\"%s\"" k v

    let parseAttributes =
        List.fold (fun acc x -> sprintf "%s %s" acc (parseAttribute x)) ""

    let rec getPropertyName b =
        match b with
        | Lambda (_,e) -> getPropertyName e
        | PropertyGet (_, info, _) -> info.Name
        | _ ->
            log "getPropertyName" <| sprintf "bad expression %A" b
            failwith "bad expression"

    let parseBinding b =
        getPropertyName b
        |> sprintf @"{Binding Path=%s}"

    let rec parseDataTemplate (DataTemplate x) =
        sprintf @"<DataTemplate>%s</DataTemplate>" (parseFrameworkElement x)

    and parseFrameworkElement x =
        match x with
        | Label (s, attrs) -> sprintf @"<Label Content=""%s"" %s/>" s
                                              (parseAttributes attrs)
        | Button s -> sprintf @"<Button Content=""%s""/>" s
        | TextBox (attrs, b) -> sprintf @"<TextBox %s Text=""%s""/>"
                                        (parseAttributes attrs)
                                        (parseBinding b)
        | PasswordBox (name, attrs,b) ->
            let name = getPropertyName b
            sprintf @"<PasswordBox x:Name=""%s"" %s />"
                name
                (parseAttributes attrs)

        | StackPanel (xs, attrs, orient) ->
                 let (+) x y = sprintf "%s\n%s" x y
                 sprintf @"<StackPanel Orientation = ""%s"" %s>%s
                           </StackPanel>"
                         (parseOrientation orient)
                         (parseAttributes attrs)
                         (List.fold (fun acc x -> acc + (parseFrameworkElement x)) "" xs)
        | Border (c, x) -> sprintf @"<Border BorderBrush=""%s""
                                             BorderThickness=""2"">%s</Border>"
                                   (parseColor c)
                                   (parseFrameworkElement x)
        | ItemsControl t -> 
                 sprintf @"<ItemsControl ItemsSource=""{Binding .}""> 
                           <ItemsControl.ItemTemplate>%s</ItemsControl.ItemTemplate>
                           </ItemsControl>" (parseDataTemplate t)

    let parseWindow (Window (c, attrs)) =
                sprintf @"<Window %s>%s</Window>"
                        (parseAttributes attrs)
                        (parseFrameworkElement c)
    let defaultDeclarations = 
        sprintf """ xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                   xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml" """

    let parseApplication (Application (c,nsMap)) =
        let namespaces =
            nsMap
            |> Map.keys
            |> Seq.map(fun name ->
                sprintf "xmlns:%s=\"%s\"" name nsMap.[name]
            )
            |> delimit (sprintf "%s    " Environment.NewLine)
        sprintf @"<Application %s>
                   <Application.MainWindow>%s</Application.MainWindow></Application>"
            namespaces
            (parseWindow c)
