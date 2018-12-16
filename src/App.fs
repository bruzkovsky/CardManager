module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Browser = Fable.Import.Browser

// MODEL

let user = "Matthias"

type Task =
    | NewTask of string
    | AcceptedTask of string * string
    | FinishedTask of string

type Model =
    { TaskForm : string
      Tasks : Task list }


type Msg =
| UpdateTaskForm of string
| CreateTask
| AcceptTask of string
| FinishTask of string
| UnbumpTask of string
| RemoveTask of string

let init() : Model =
    { TaskForm = ""
      Tasks = [] }

// UPDATE

let accept (title : string) (d : Task) =
    match d with
    | NewTask t ->
        if t = title then
            sprintf "Task %s was accepted!" t
            |> Browser.console.log
            (AcceptedTask (t, user))
        else d
    | AcceptedTask (_, _) ->
        // if t = title then
        //     sprintf "Task %s has now %d bumps!" t b
        //     |> Browser.console.log
        //     (AcceptedTask (t, b + 1))
        // else d
        d
    | FinishedTask _ -> d

let unassign (title : string) (d : Task) =
    match d with
    | NewTask _ -> d
    | AcceptedTask (t, _) ->
        NewTask t
    | FinishedTask _ -> d

let finish (title : string) (d : Task) =
    match d with
    | NewTask t -> d
    | AcceptedTask (t, _) ->
        if t = title then (FinishedTask t) else d
    | FinishedTask _ -> d

let sortTasks( tasks : Task list)=
    tasks |> List.sortByDescending(fun elem -> 
            match elem with
               | NewTask _-> 0
               | AcceptedTask _ -> 0
               | FinishedTask _ -> -1          
            )

let update (msg:Msg) (model:Model) =
    match msg with
    | UpdateTaskForm content ->
        { model with TaskForm = content }
    | CreateTask ->
        let newTask = NewTask model.TaskForm
        { model with
            TaskForm = ""
            Tasks = newTask::model.Tasks |> sortTasks }
    | AcceptTask title ->
        let tasks = model.Tasks |> List.map (accept title)
        { model with Tasks = tasks |> sortTasks}
    | FinishTask title ->
        let tasks = 
            model.Tasks
            |> List.map (finish title)
        { model with Tasks = tasks |> sortTasks }
    | UnbumpTask title ->
        let tasks =
            model.Tasks
            |> List.map (unassign title)
        { model with Tasks = tasks |> sortTasks }
    | RemoveTask title ->
        let tasks = model.Tasks |> List.choose (fun elem ->
                match elem with
                | NewTask t -> if t = title then None else Some(elem)
                | FinishedTask t -> if t = title then None else Some(elem)
                | _ -> Some(elem))
        printfn "%A" tasks  |> Browser.console.log
        { model with Tasks = tasks |> sortTasks }

// VIEW (rendered with React)

open Fulma

let newTaskTile dispatch (title : string) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is12; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str title ] ]
              Card.content []
                [ Content.content [] [ str "A new task." ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> AcceptTask title |> dispatch) ] ] [ str "Accept" ]
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> RemoveTask title |> dispatch) ] ] [ str "Remove" ] 
                  ] ] ]

let finishedTaskTile dispatch (title : string) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is12; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str title ] ]
              Card.content []
                [ Content.content [] [ str "You have finished this task. Well done! 👏" ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> RemoveTask title |> dispatch) ] ]
                    [ str "Remove" ]               
                ] ] ]

let acceptedTaskTile dispatch (title : string) (assignee : string) =
    let text = sprintf "This task was accepted by %s" assignee
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is12; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str title ] ]
              Card.content []
                [ Content.content [] [ str text ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> FinishTask title |> dispatch) ] ]
                    [ str "Finish" ]
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> UnbumpTask title |> dispatch) ] ]
                    [ str "Move back" ]
                ] ] ]

let toCard dispatch (task : Task) =
    match task with
    | NewTask title ->
        newTaskTile dispatch title
    | AcceptedTask (title, bumps) ->
        acceptedTaskTile dispatch title bumps
    | FinishedTask title ->
        finishedTaskTile dispatch title

let toCardRow row =
    Tile.tile [ Tile.IsParent; Tile.Size Tile.Is12 ] row

let rec chunkByTwo soFar l =
    match l with
    | x1::[x2] ->
        [x1; x2]::soFar
    | x1::x2::xs ->
        chunkByTwo ([x1; x2]::soFar) xs
    | xs ->
        xs::soFar

let toCardRows dispatch (titles : Task list) =
    titles
    // |> chunkByTwo []
    |> List.rev
    // |> List.map ((List.map (toCard dispatch)) >> toCardRow)
    |> List.map (toCard dispatch)

let view (model:Model) dispatch =   
    div []
      [ Navbar.navbar [ Navbar.Color IsBlack ]
            [ Navbar.Brand.div []
                [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                    [ str "Card Manager" ] ] ]
        Columns.columns [] [
            Column.column [ Column.Width (Screen.All, Column.Is4) ] [
                Hero.hero [ Hero.Color IsInfo ] [
                      Hero.body [] [
                        Heading.h1 [ ] [ str "New tasks" ] ] ]
                Container.container [ Container.IsFluid ]
                  [ Tile.tile [ Tile.IsAncestor; Tile.IsVertical ]
                            [ yield Tile.tile [ Tile.IsChild; Tile.Size Tile.Is12 ]
                                [ Tile.tile [ Tile.IsChild ]
                                    [ Card.card []
                                        [ Card.header []
                                            [ Card.Header.title [] [ str "Write a task!" ] ]
                                          Card.content []
                                            [ Input.text [ Input.Placeholder "Your task"
                                                           Input.Value model.TaskForm
                                                           Input.OnChange (fun ev -> UpdateTaskForm ev.Value |> dispatch)
                                                           Input.Option.Props
                                                             [ OnKeyUp (fun key -> if key.which = 13.0 then dispatch CreateTask)  ] ] ]
                                          Card.footer []
                                            [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> dispatch CreateTask) ] ]
                                                [ str "Submit" ] ] ] ] ]
                              yield! model.Tasks
                                |> List.filter (fun x ->
                                  match x with
                                    | NewTask _ -> true
                                    | _ -> false)
                                |> toCardRows dispatch ] ] ]
            Column.column [ Column.Width (Screen.All, Column.Is4) ] [
                Hero.hero [ Hero.Color IsWarning ] [
                      Hero.body [] [
                        Heading.h1 [ ] [ str "Running tasks" ] ] ] 
                Container.container [ Container.IsFluid ]
                  [ Tile.tile [ Tile.IsAncestor; Tile.IsVertical ]
                      [ yield! model.Tasks
                                |> List.filter (fun x ->
                                  match x with
                                    | AcceptedTask _ -> true
                                    | _ -> false)
                                |> toCardRows dispatch ] ] ]
            Column.column [ Column.Width (Screen.All, Column.Is4) ] [
                Hero.hero [ Hero.Color IsSuccess ] [
                      Hero.body [] [
                        Heading.h1 [ ] [ str "Finished tasks" ] ] ]
                Container.container [ Container.IsFluid ]
                  [ Tile.tile [ Tile.IsAncestor; Tile.IsVertical ]
                      [ yield! model.Tasks
                                |> List.filter (fun x ->
                                  match x with
                                    | FinishedTask _ -> true
                                    | _ -> false)
                                |> toCardRows dispatch ] ] ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

// App
Program.mkSimple init update view
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
// |> Program.withConsoleTrace
|> Program.withDebugger
#endif
|> Program.run
