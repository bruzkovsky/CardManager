module App

open System
open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Browser = Fable.Import.Browser

let mutable user = "Matthias"

type TaskModel = 
    {
        Id : Guid
        Title : string
        Description : string
    }

type Task =
    | NewTask of TaskModel
    | BumpedTask of TaskModel * int
    | RejectedTask of TaskModel
    | AcceptedTask of TaskModel * string
    | FinishedTask of TaskModel

type Model =
    { TaskForm : TaskModel
      Tasks : Task list
      CurrentSearch : string
      CurrentUser : string
     }


type Msg =
| UpdateTaskForm of string * string
| CreateTask
| BumpTask of System.Guid
| AcceptTask of System.Guid
| FinishTask of System.Guid
| RejectTask of System.Guid
| UnbumpTask of System.Guid
| RemoveTask of System.Guid
| Search of string
| UpdateUser of string

let init() : Model =
    {
      TaskForm = {
          Id = Guid.NewGuid()
          Title = ""
          Description = ""
        }          
      Tasks = []
      CurrentSearch = ""
      CurrentUser = "Matthias"
    }

// UPDATE

let bump (taskId : System.Guid) (d : Task) =
    match d with
    | NewTask t ->
        if t.Id = taskId then
            sprintf "Task %s has its first bump!" t.Title
            |> Browser.console.log
            (BumpedTask (t, 1))
        else d
    | BumpedTask (t, b) ->
        if t.Id = taskId then
            sprintf "Task %s has now %d bumps!" t.Title b
            |> Browser.console.log
            (BumpedTask (t, b + 1))
        else d
    | RejectedTask _ -> d
    | _ -> d

let unbump (taskId : System.Guid) (d : Task) =
    match d with
    | NewTask _ -> d
    | BumpedTask (t, b) when b > 1 ->
        if t.Id = taskId then
            (BumpedTask (t, b - 1))
        else d
    | BumpedTask (t, _) ->
        NewTask t
    | RejectedTask _ -> d
    | AcceptedTask ( t , _) -> 
        if t.Id = taskId then
            (BumpedTask (t, 0))
        else d
    | _ -> d

let reject (taskId : System.Guid) (d : Task) =
    match d with
    | NewTask t ->
        if t.Id = taskId then (RejectedTask t) else d
    | BumpedTask _ -> d
    | RejectedTask _ -> d
    | _ -> d

let accept (taskId : System.Guid) (d : Task) =
    match d with
    | NewTask t -> if t.Id = taskId then (AcceptedTask (t , user)) else d    
    | BumpedTask (t , _) -> if t.Id = taskId then (AcceptedTask (t , user)) else d  
    | _ -> d

let finish (taskId : System.Guid) (d : Task) =
    match d with
    | AcceptedTask (t , _) -> if t.Id = taskId then (FinishedTask t ) else d     
    | _ -> d

let sortTasks( tasks : Task list)=
    tasks |> List.sortByDescending(fun elem -> 
            match elem with
               | NewTask _-> 0
               | BumpedTask (t, b) -> b 
               | RejectedTask _ -> -1 
               | _ -> 0         
            )

let update (msg:Msg) (model:Model) =
    match msg with
    | UpdateTaskForm (value, prop) ->
        match prop with
        | "title" ->{ model with TaskForm = {model.TaskForm with Title = value} }
        | "description" ->{ model with TaskForm = {model.TaskForm with Description = value} }
        | _ -> model
    | CreateTask ->
        let newTask = NewTask model.TaskForm
        { model with
            TaskForm = {
                Id = Guid.NewGuid()
                Title = ""
                Description = ""}
            Tasks = newTask::model.Tasks |> sortTasks }
    | BumpTask task ->
        let tasks = model.Tasks |> List.map (bump task)
        { model with Tasks = tasks |> sortTasks}
    | RejectTask task ->
        let tasks = model.Tasks |> List.map (reject task)
        { model with Tasks = tasks |> sortTasks }
    | UnbumpTask task ->
        let tasks = model.Tasks |> List.map (unbump task)
        { model with Tasks = tasks |> sortTasks }
    | RemoveTask task ->
        let tasks = model.Tasks |> List.choose (fun elem ->
                match elem with
                | RejectedTask d -> if d.Id = task then None else Some(elem)
                | FinishedTask d -> if d.Id = task then None else Some(elem)
                | _ -> Some(elem))
        printfn "%A" tasks  |> Browser.console.log
        { model with Tasks = tasks |> sortTasks }
    | Search searchText ->
        { model with CurrentSearch = searchText }
    | AcceptTask task ->
        let tasks = model.Tasks |> List.map (accept task)
        { model with Tasks = tasks |> sortTasks }
    | FinishTask task ->   
        let tasks = model.Tasks |> List.map (finish task)
        { model with Tasks = tasks |> sortTasks }
    | UpdateUser u -> 
        user <- u
        sprintf "Task %s " user |> Browser.console.log
        { model with CurrentUser = u  }


// VIEW (rendered with React)

open Fulma

let newTaskTile dispatch (task : TaskModel) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is12; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str task.Title ] ]
              Card.content []
                [ Content.content [] [ str task.Description ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> BumpTask task.Id |> dispatch) ] ] [ str "Bump" ]
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> RejectTask task.Id |> dispatch) ] ] [ str "Reject" ] 
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> AcceptTask task.Id |> dispatch) ] ] [ str "Accept" ]
                  ] ] ]

let rejectedTaskTile dispatch (task : TaskModel) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is12; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str task.Title ] ]
              Card.content []
                [ Content.content [] [ str "Unfortunately this task has been rejected 🙁" ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> RemoveTask task.Id |> dispatch) ] ]
                    [ str "Remove" ]               
                ] ] ]

let bumpedTaskTile dispatch (task : TaskModel) (bumps : int) =
    let text = sprintf "Rating: %d" bumps
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is12; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str task.Title ] 
                  Content.content [] [ str text ]]
              Card.content []
                [ Content.content [] [ str task.Description ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> BumpTask task.Id |> dispatch) ] ] [ str "Bump" ]
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> UnbumpTask task.Id |> dispatch) ] ] [ str "UmBump" ]
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> AcceptTask task.Id |> dispatch) ] ] [ str "Accept" ]
                ] ] ]

let acceptedTaskTile dispatch (task : TaskModel)  (assignee : string) =
    let text = sprintf "%s %s This task was accepted by %s" task.Description System.Environment.NewLine assignee
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is12; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str task.Title ] ]
              Card.content []
                [ Content.content [] [ str (task.Description + "\n" + text) ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> FinishTask task.Id |> dispatch) ] ] [ str "Finish" ]
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> UnbumpTask task.Id |> dispatch) ] ]  [ str "Move back" ]
                ] ] ]

let finishedTaskTile dispatch (task : TaskModel)  =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is12; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str task.Title ] ]
              Card.content []
                [ Content.content [] [ str "You have finished this task. Well done! 👏" ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> RemoveTask task.Id |> dispatch) ] ] [ str "Remove" ]               
                ] ] ]

let toCard dispatch (task : Task) =
    match task with
    | NewTask title ->
        newTaskTile dispatch title
    | BumpedTask (title, bumps) ->
        bumpedTaskTile dispatch title bumps
    | RejectedTask title ->
        rejectedTaskTile dispatch title
    | AcceptedTask (d , u) -> 
        acceptedTaskTile dispatch d u
    | FinishedTask d -> 
        finishedTaskTile dispatch d 

let toCardRow row =
    Tile.tile [ Tile.IsParent; Tile.Size Tile.Is12 ] row

let getVisibleTiles  (model : Model) = 
    model.Tasks |> List.choose (fun elem ->
        match elem with
        |  NewTask d ->
            if d.Title.IndexOf(model.CurrentSearch) <> -1 || d.Description.IndexOf(model.CurrentSearch) <> -1 then
                Some(elem)
            else
                None
        |  BumpedTask (d , _) ->
            if d.Title.IndexOf(model.CurrentSearch) <> -1 || d.Description.IndexOf(model.CurrentSearch) <> -1 then
                Some(elem)
            else
                None          
        |  RejectedTask d  ->
            if d.Title.IndexOf(model.CurrentSearch) <> -1 || d.Description.IndexOf(model.CurrentSearch) <> -1 then
                Some(elem)
            else
                None                  
        |  AcceptedTask (d , _)  ->
            if d.Title.IndexOf(model.CurrentSearch) <> -1 || d.Description.IndexOf(model.CurrentSearch) <> -1 then
                Some(elem)
            else
                None           
        |  FinishedTask d  ->
            if d.Title.IndexOf(model.CurrentSearch) <> -1 || d.Description.IndexOf(model.CurrentSearch) <> -1 then
                Some(elem)
            else
                None           
        )

let toCardRows dispatch (titles : Task list) =
    titles
    |> List.rev
    |> List.map (toCard dispatch)

let newTasksColumn (model:Model) dispatch =
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
                                            [ 
                                            Input.text [ 
                                                Input.Placeholder "Your task"
                                                Input.Value model.TaskForm.Title
                                                Input.OnChange (fun ev -> UpdateTaskForm (ev.Value, "title") |> dispatch)
                                                Input.Option.Props [ OnKeyUp (fun key -> if key.which = 13.0 then dispatch CreateTask)  ] ]
                                            Input.text [ 
                                                Input.Placeholder "Decription"
                                                Input.Value model.TaskForm.Description
                                                Input.OnChange (fun ev -> UpdateTaskForm (ev.Value, "description") |> dispatch)
                                                Input.Option.Props [ OnKeyUp (fun key -> if key.which = 13.0 then dispatch CreateTask)  ] ]     
                                            ]
                                          Card.footer []
                                            [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> dispatch CreateTask) ] ]
                                                [ str "Submit" ] ] ] ] ]
                              yield! model |> getVisibleTiles
                                |> List.filter (fun x ->
                                  match x with
                                    | NewTask _ -> true
                                    | BumpedTask _ -> true
                                    | _ -> false)
                                |> toCardRows dispatch ] ] ]

let runningTasksColumn (model:Model) dispatch =
    Column.column [ Column.Width (Screen.All, Column.Is4) ] [
                Hero.hero [ Hero.Color IsWarning ] [
                      Hero.body [] [
                        Heading.h1 [ ] [ str "Running tasks" ] ] ] 
                Container.container [ Container.IsFluid ]
                  [ Tile.tile [ Tile.IsAncestor; Tile.IsVertical ]
                      [ yield! model |> getVisibleTiles
                                |> List.filter (fun x ->
                                  match x with
                                    | AcceptedTask _ -> true
                                    | _ -> false)
                                |> toCardRows dispatch ] ] ]

let finishedTasksColumn (model:Model) dispatch =
    Column.column [ Column.Width (Screen.All, Column.Is4) ] [
                Hero.hero [ Hero.Color IsSuccess ] [
                      Hero.body [] [
                        Heading.h1 [ ] [ str "Finished tasks" ] ] ]
                Container.container [ Container.IsFluid ]
                  [ Tile.tile [ Tile.IsAncestor; Tile.IsVertical ]
                      [ yield! model |> getVisibleTiles
                                |> List.filter (fun x ->
                                  match x with
                                    | FinishedTask _ -> true
                                    | _ -> false)
                                |> toCardRows dispatch ] ] ]

let view (model:Model) dispatch =   
    div []
      [ Navbar.navbar [ Navbar.Color IsBlack ]
            [ Navbar.Brand.div []
                [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                    [ str "Task Manager" ] ]
              
              Navbar.End.div [ ]
                [Navbar.Item.a  [   Navbar.Item.HasDropdown
                                    Navbar.Item.IsHoverable ]
                [ 
                  Input.text [ 
                    Input.Placeholder "Search"
                    Input.Value model.CurrentSearch
                    Input.OnChange (fun ev -> Search ev.Value |> dispatch)
                    ]  
                  Navbar.Link.a [ ] [ str user ]
                  Navbar.Dropdown.div [ ]
                    [ Navbar.Item.a [ Navbar.Item.Props [ OnClick (fun ev -> UpdateUser "Matthias" |> dispatch)] ]
                        [ str "Matthias" ]
                      Navbar.Item.a [ Navbar.Item.Props [ OnClick (fun ev -> UpdateUser "Jürgen" |> dispatch)] ]
                        [ str "Jürgen" ]
                      Navbar.Item.a [ Navbar.Item.Props [ OnClick (fun ev -> UpdateUser "Tanja" |> dispatch)] ]
                        [ str "Tanja" ] ] ]]]

        Columns.columns [] [
            newTasksColumn model dispatch
            runningTasksColumn model dispatch
            finishedTasksColumn model dispatch ] ]

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
