module App

open System
open Elmish
open Elmish.React
open Feliz

type Todo =
    {
        Description: string
        Completed: bool
        Id: Guid
    }

type TodoBeingEdited =
    {
        Id: Guid
        NewText: string
    }

type Filter = Complete | Incomplete

type State = 
    { 
        Todos: Todo list
        NewTodo: string 
        BeingEdited: TodoBeingEdited list
        ActiveFilter: Filter option
    }

type Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleComplete of Guid
    | DeleteTodo of Guid
    | StartEdit of Guid
    | CancelEdit of Guid
    | ConfirmEdit of Guid
    | SetNewEditText of Guid * string
    | SetFilter of Filter
    | RemoveFilter

let init() =
    { 
        Todos = [ { Id = Guid.NewGuid(); Description = "Learn F#"; Completed = false } ]
        NewTodo = ""
        BeingEdited = []
        ActiveFilter = None
    }

let update (msg: Msg) (state: State): State =
    match msg with

    | SetNewTodo desc -> { state with NewTodo = desc }

    | DeleteTodo id -> 
        { 
            state with 
                Todos =  
                    state.Todos
                    |> List.filter (fun t -> t.Id <> id)
        }

    | ToggleComplete id ->
        {
            state with 
                Todos =
                    state.Todos
                    |> List.map (fun t -> 
                        if t.Id = id 
                        then { t with Completed = not t.Completed }
                        else t )
        }

    | AddNewTodo when state.NewTodo = "" -> state

    | AddNewTodo -> 
        let newTodo =
            { 
                Description = state.NewTodo; 
                Completed = false
                Id = Guid.NewGuid()
            }
        { 
            state with 
                Todos = List.append state.Todos [newTodo]
                NewTodo = ""
                ActiveFilter = 
                    if state.ActiveFilter = Some Incomplete then state.ActiveFilter else None
        }

    | StartEdit id ->
        let newEdit = 
            state.Todos
            |> List.tryFind (fun t -> t.Id = id)
            |> Option.map (fun t -> { Id = id; NewText = t.Description })
        { 
            state with 
                BeingEdited =
                    match newEdit with
                    | Some edit -> List.append [edit] state.BeingEdited
                    | None -> state.BeingEdited
        }

    | CancelEdit id -> 
        { 
            state with
                BeingEdited = 
                    state.BeingEdited
                    |> List.filter (fun e -> e.Id <> id)
        }

    | SetNewEditText (id, input) ->
        { 
            state with 
                BeingEdited = 
                    state.BeingEdited
                    |> List.map (fun e -> 
                        if e.Id = id then { e with NewText = input }
                        else e )
        }

    | ConfirmEdit id -> 
        let edit = 
            state.BeingEdited
            |> List.tryFind (fun e -> e.Id = id)

        match edit with
        | None -> state
        | Some beingEdited when beingEdited.NewText = "" -> state 
        | Some beingEdited ->
            {
                state with
                    Todos =
                        state.Todos
                        |> List.map (fun t ->
                            if t.Id = beingEdited.Id then
                                { t with Description = beingEdited.NewText } 
                            else t )
                    BeingEdited =
                        state.BeingEdited
                        |> List.filter (fun e -> e.Id <> beingEdited.Id)
            }

    | SetFilter filter -> 
        { 
            state with 
                ActiveFilter = Some filter
                BeingEdited = []
        }

    | RemoveFilter -> 
        {
            state with 
                ActiveFilter = None 
                BeingEdited = []
        }

let appTitle = 
    Html.p [
        prop.className B.Title
        prop.text "Elmish todo list"
    ]

let inputField (state : State) (dispatch : Msg -> unit) =
    Html.div [
        prop.classes [ B.Field; B.HasAddons ]
        prop.children [
            Html.div [
                prop.classes [ B.Control; B.IsExpanded ]
                prop.children [
                    Html.input [
                        prop.classes [ B.Input; B.IsMedium ]
                        prop.valueOrDefault state.NewTodo
                        prop.onChange (SetNewTodo >> dispatch)
                    ]
                ]
            ]
            Html.div [
                prop.className B.Control
                prop.children [
                    Html.button [
                        prop.classes [ B.Button; B.IsPrimary; B.IsMedium ]
                        prop.onClick (fun _ -> dispatch AddNewTodo)
                        prop.children [
                            Html.i [
                                prop.classes [ FA.Fa; FA.FaPlus ]
                            ]
                        ]
                    ]
                ]   
            ]
        ]
    ]

let div (classes : string list) (children : Fable.React.ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ] 

let noItems =
    Html.ul [
        Html.div [ 
            prop.className B.Box
            prop.text "No items"
        ]
    ]

let renderTodo (todo : Todo) (dispatch : Msg -> unit) =
    div [ B.Box ] [
        div [ B.Columns; B.IsMobile; B.IsVcentered ] [
            div [ B.Column ] [
                Html.p [
                    prop.className B.Subtitle
                    prop.text todo.Description
                ]
            ]
            div [ B.Column; B.IsNarrow ] [
                div [ B.Buttons ] [
                    Html.button [
                        prop.classes [ B.Button; if todo.Completed then B.IsSuccess ]
                        prop.onClick (fun _ -> dispatch (ToggleComplete todo.Id))
                        prop.children [
                            Html.i [ prop.classes [ FA.Fa; FA.FaCheck ] ]
                        ]
                    ]
                    Html.button [
                        prop.classes [ B.Button; B.IsPrimary ]
                        prop.onClick (fun _ -> dispatch (StartEdit todo.Id))
                        prop.children [
                            Html.i [ prop.classes [ FA.Fa; FA.FaCheck ] ]
                        ]
                    ]
                    Html.button [
                        prop.classes [ B.Button; B.IsDanger ]
                        prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
                        prop.children [
                            Html.i [ prop.classes [ FA.Fa; FA.FaCheck ] ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let renderEditForm (beingEdited : TodoBeingEdited) (todo : Todo) (dispatch : Msg -> unit) =
    div [ B.Box ] [
        div [ B.Field; B.IsGrouped ] [
            div [ B.Control; B.IsExpanded ] [
                Html.input [
                    prop.classes [ B.Input; B.IsMedium ]
                    prop.valueOrDefault beingEdited.NewText
                    prop.onTextChange (fun s -> SetNewEditText (todo.Id, s) |> dispatch)
                ]
            ]
        ]
        div [ B.Control; B.Buttons ] [
            Html.button [
                prop.classes [ 
                    B.Button 
                    if beingEdited.NewText <> todo.Description then B.IsPrimary else B.IsOutlined
                ]
                prop.onClick (fun _ -> ConfirmEdit todo.Id |> dispatch)
                prop.children [
                    Html.i [ prop.classes [ FA.Fa; FA.FaSave ] ]
                ]
            ]   
            Html.button [
                prop.classes [ B.Button; B.IsWarning ]
                prop.onClick (fun _ -> CancelEdit todo.Id |> dispatch)
                prop.children [
                    Html.i [ prop.classes [FA.Fa; FA.FaSave ] ]
                ]
            ]
        ]
    ]

let todoList (state : State) (dispatch : Msg -> unit) =
    let toShow = 
        match state.ActiveFilter with
        | Some f -> match f with
                    | Complete -> List.filter (fun t -> t.Completed = true) state.Todos
                    | Incomplete -> List.filter (fun t -> t.Completed = false) state.Todos
        | None -> state.Todos
    
    if toShow = [] then noItems else
        Html.ul [
            prop.children [
                    for todo in toShow ->
                        let beingEdited =
                            state.BeingEdited
                            |> List.tryFind (fun e -> e.Id = todo.Id)
                        match beingEdited with
                        | Some edit -> renderEditForm edit todo dispatch
                        | None -> renderTodo todo dispatch
            ]
        ]

let renderFilterTabs (state : State) (dispatch : Msg -> unit) =
    div [ B.Tabs; B.IsToggle; B.IsFullwidth ] [
        Html.ul [
            Html.li [
                prop.classes [ if state.ActiveFilter = None then B.IsActive ]
                prop.onClick (fun _ -> dispatch RemoveFilter)
                prop.children [
                    Html.a [
                        prop.text "All"
                    ]
                ]
            ]
            Html.li [
                prop.classes [ if state.ActiveFilter = Some Complete then B.IsActive ]
                prop.onClick (fun _ -> dispatch (SetFilter Complete))
                prop.children [
                    Html.a [
                        prop.text "Complete"
                    ]
                ]
            ]
            Html.li [
                prop.classes [ if state.ActiveFilter = Some Incomplete then B.IsActive ]
                prop.onClick (fun _ -> dispatch (SetFilter Incomplete))
                prop.children [
                    Html.a [
                        prop.text "Incomplete"
                    ]
                ]
            ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.style [ style.padding 20 ]
        prop.children [
            appTitle
            inputField state dispatch
            renderFilterTabs state dispatch
            todoList state dispatch
        ]
    ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run