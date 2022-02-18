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
        BeingEdited: TodoBeingEdited option
        ActiveFilter: Filter option
    }

type Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleComplete of Guid
    | DeleteTodo of Guid
    | StartEdit of Guid
    | CancelEdit
    | ConfirmEdit
    | SetNewEditText of string
    | SetFilter of Filter
    | RemoveFilter

let init() =
    { 
        Todos = [ { Id = Guid.NewGuid(); Description = "Learn F#"; Completed = false } ]
        NewTodo = ""
        BeingEdited = None
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
        { 
            state with 
                BeingEdited = 
                    state.Todos
                    |> List.tryFind (fun t -> t.Id = id)
                    |> Option.map (fun t -> { Id = id; NewText = t.Description })
        }

    | CancelEdit -> 
        { 
            state with
                BeingEdited = None
        }

    | SetNewEditText s ->
        { 
            state with 
                BeingEdited = 
                    state.BeingEdited
                    |> Option.map (fun t -> { t with NewText = s })
        }

    | ConfirmEdit -> 
        match state.BeingEdited with
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
                    BeingEdited = None
            }

    | SetFilter filter -> 
        { 
            state with 
                ActiveFilter = Some filter
                BeingEdited = None
        }

    | RemoveFilter -> { state with ActiveFilter = None }

let appTitle = 
    Html.p [
        prop.className "title"
        prop.text "Elmish todo list"
    ]

let inputField (state : State) (dispatch : Msg -> unit) =
    Html.div [
        prop.classes [ "field"; "has-addons" ]
        prop.children [
            Html.div [
                prop.classes [ "control"; "is-expanded" ]
                prop.children [
                    Html.input [
                        prop.classes [ "input"; "is-medium" ]
                        prop.valueOrDefault state.NewTodo
                        prop.onChange (SetNewTodo >> dispatch)
                    ]
                ]
            ]
            Html.div [
                prop.className "control"
                prop.children [
                    Html.button [
                        prop.classes [ "button"; "is-primary"; "is-medium" ]
                        prop.onClick (fun _ -> dispatch AddNewTodo)
                        prop.children [
                            Html.i [
                                prop.classes [ "fa"; "fa-plus" ]
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
            prop.className "box"
            prop.text "No items"
        ]
    ]

let renderTodo (todo : Todo) (dispatch : Msg -> unit) =
    div [ "box" ] [
        div [ "columns"; "is-mobile"; "is-vcentered" ] [
            div [ "column" ] [
                Html.p [
                    prop.className "subtitle"
                    prop.text todo.Description
                ]
            ]
            div [ "column"; "is-narrow" ] [
                div [ "buttons" ] [
                    Html.button [
                        prop.classes [ "button"; if todo.Completed then "is-success" ]
                        prop.onClick (fun _ -> dispatch (ToggleComplete todo.Id))
                        prop.children [
                            Html.i [ prop.classes [ "fa"; "fa-check" ] ]
                        ]
                    ]
                    Html.button [
                        prop.classes [ "button"; "is-primary" ]
                        prop.onClick (fun _ -> dispatch (StartEdit todo.Id))
                        prop.children [
                            Html.i [ prop.classes [ "fa"; "fa-check" ] ]
                        ]
                    ]
                    Html.button [
                        prop.classes [ "button"; "is-danger" ]
                        prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
                        prop.children [
                            Html.i [ prop.classes [ "fa"; "fa-check" ] ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let renderEditForm (beingEdited : TodoBeingEdited) (todo : Todo) (dispatch : Msg -> unit) =
    div [ "box" ] [
        div [ "field"; "is-grouped" ] [
            div [ "control"; "is-expanded" ] [
                Html.input [
                    prop.classes [ "input"; "is-medium" ]
                    prop.valueOrDefault beingEdited.NewText
                    prop.onTextChange (SetNewEditText >> dispatch)
                ]
            ]
        ]
        div [ "control"; "buttons" ] [
            Html.button [
                prop.classes [ 
                    "button" 
                    if beingEdited.NewText <> todo.Description then "is-primary" else "is-outlined"
                ]
                prop.onClick (fun _ -> dispatch ConfirmEdit)
                prop.children [
                    Html.i [ prop.classes ["fa"; "fa-save" ] ]
                ]
            ]   
            Html.button [
                prop.classes [ "button"; "is-warning" ]
                prop.onClick (fun _ -> dispatch CancelEdit)
                prop.children [
                    Html.i [ prop.classes ["fa"; "fa-save" ] ]
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
                        match state.BeingEdited with
                        | Some beingEdited when beingEdited.Id = todo.Id ->
                            renderEditForm beingEdited todo dispatch
                        | _ -> renderTodo todo dispatch
            ]
        ]

let renderFilterTabs (state : State) (dispatch : Msg -> unit) =
    div [ "tabs"; "is-toggle"; "is-fullwidth" ] [
        Html.ul [
            Html.li [
                prop.classes [ if state.ActiveFilter = None then "is-active" ]
                prop.onClick (fun _ -> dispatch RemoveFilter)
                prop.children [
                    Html.a [
                        prop.text "All"
                    ]
                ]
            ]
            Html.li [
                prop.classes [ if state.ActiveFilter = Some Complete then "is-active" ]
                prop.onClick (fun _ -> dispatch (SetFilter Complete))
                prop.children [
                    Html.a [
                        prop.text "Complete"
                    ]
                ]
            ]
            Html.li [
                prop.classes [ if state.ActiveFilter = Some Incomplete then "is-active" ]
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