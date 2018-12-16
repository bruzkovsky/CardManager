module App

open System
open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Browser = Fable.Import.Browser

type DraftModel = 
    {
        Id : Guid
        Title : string
        Description : string
    }

type Draft =
    | NewDraft of DraftModel
    | BumpedDraft of DraftModel * int
    | RejectedDraft of DraftModel

type Model =
    { DraftForm : DraftModel
      Drafts : Draft list }


type Msg =
| UpdateDraftForm of string * string
| CreateDraft
| BumpDraft of System.Guid
| RejectDraft of System.Guid
| UnbumpDraft of System.Guid
| RemoveDraft of System.Guid

let init() : Model =
    {
      DraftForm = {
          Id = Guid.NewGuid()
          Title = ""
          Description = ""}
      Drafts = [] }

// UPDATE

let bump (draftId : System.Guid) (d : Draft) =
    match d with
    | NewDraft t ->
        if t.Id = draftId then
            sprintf "Draft %s has its first bump!" t.Title
            |> Browser.console.log
            (BumpedDraft (t, 1))
        else d
    | BumpedDraft (t, b) ->
        if t.Id = draftId then
            sprintf "Draft %s has now %d bumps!" t.Title b
            |> Browser.console.log
            (BumpedDraft (t, b + 1))
        else d
    | RejectedDraft _ -> d

let unbump (draftId : System.Guid) (d : Draft) =
    match d with
    | NewDraft _ -> d
    | BumpedDraft (t, b) when b > 1 ->
        if t.Id = draftId then
            (BumpedDraft (t, b - 1))
        else d
    | BumpedDraft (t, _) ->
        NewDraft t
    | RejectedDraft _ -> d

let reject (draftId : System.Guid) (d : Draft) =
    match d with
    | NewDraft t ->
        if t.Id = draftId then (RejectedDraft t) else d
    | BumpedDraft _ -> d
    | RejectedDraft _ -> d

let sortDrafts( drafts : Draft list)=
    drafts |> List.sortByDescending(fun elem -> 
            match elem with
               | NewDraft _-> 0
               | BumpedDraft (t, b) -> b 
               | RejectedDraft _ -> -1          
            )

let update (msg:Msg) (model:Model) =
    match msg with
    | UpdateDraftForm (value, prop) ->
        match prop with
        | "title" ->{ model with DraftForm = {model.DraftForm with Title = value} }
        | "description" ->{ model with DraftForm = {model.DraftForm with Description = value} }
    | CreateDraft ->
        let newDraft = NewDraft model.DraftForm
        { model with
            DraftForm = {
                Id = Guid.NewGuid()
                Title = ""
                Description = ""}
            Drafts = newDraft::model.Drafts |> sortDrafts }
    | BumpDraft draft ->
        let drafts = model.Drafts |> List.map (bump draft)
        { model with Drafts = drafts |> sortDrafts}
    | RejectDraft draft ->
        let drafts = model.Drafts |> List.map (reject draft)
        { model with Drafts = drafts |> sortDrafts }
    | UnbumpDraft draft ->
        let drafts = model.Drafts |> List.map (unbump draft)
        { model with Drafts = drafts |> sortDrafts }
    | RemoveDraft draft ->
        let drafts = model.Drafts |> List.choose (fun elem ->
                match elem with
                | RejectedDraft d -> if d.Id = draft then None else Some(elem)
                | _ -> Some(elem))
        printfn "%A" drafts  |> Browser.console.log
        { model with Drafts = drafts |> sortDrafts }

// VIEW (rendered with React)

open Fulma

let newDraftTile dispatch (draft : DraftModel) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str draft.Title ] ]
              Card.content []
                [ Content.content [] [ str draft.Description ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> BumpDraft draft.Id |> dispatch) ] ] [ str "Bump" ]
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> RejectDraft draft.Id |> dispatch) ] ] [ str "Reject" ] 
                  ] ] ]

let rejectedDraftTile dispatch (draft : DraftModel) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str draft.Title ] ]
              Card.content []
                [ Content.content [] [ str "Unfortunately this draft has been rejected 🙁" ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> RemoveDraft draft.Id |> dispatch) ] ]
                    [ str "Remove" ]               
                ] ] ]

let bumpedDraftTile dispatch (draft : DraftModel) (bumps : int) =
    let text = sprintf "Rating: %d" bumps
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str draft.Title ] 
                  Content.content [] [ str text ]]
              Card.content []
                [ Content.content [] [ str draft.Description ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> BumpDraft draft.Id |> dispatch) ] ]
                    [ str "Bump" ]
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> UnbumpDraft draft.Id |> dispatch) ] ]
                    [ str "UmBump" ]
                ] ] ]

let toCard dispatch (draft : Draft) =
    match draft with
    | NewDraft title ->
        newDraftTile dispatch title
    | BumpedDraft (title, bumps) ->
        bumpedDraftTile dispatch title bumps
    | RejectedDraft title ->
        rejectedDraftTile dispatch title

let toCardRow row =
    Tile.tile [ Tile.IsParent; Tile.Size Tile.Is12 ] row

let rec chunkByThree soFar l =
    match l with
    | x1::x2::[x3] ->
        [x1; x2; x3]::soFar
    | x1::x2::x3::xs ->
        chunkByThree ([x1; x2; x3]::soFar) xs
    | xs ->
        xs::soFar

let toCardRows dispatch (titles : Draft list) =
    titles
    |> chunkByThree []
    |> List.rev
    |> List.map ((List.map (toCard dispatch)) >> toCardRow)

let view (model:Model) dispatch =   
    div []
      [ Navbar.navbar [ Navbar.Color IsBlack ]
            [ Navbar.Brand.div []
                [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                    [ str "Card Manager" ] ] ]
        Container.container [ Container.IsFluid ]
          [ h1 [ Class "is-size-1 app-title" ] [ str "Manage your Cards" ]
            Tile.tile [ Tile.IsAncestor; Tile.IsVertical ]
                [ yield Tile.tile [ Tile.IsParent; Tile.Size Tile.Is12 ]
                    [ Tile.tile [ Tile.IsChild ]
                        [ Card.card []
                            [ Card.header []
                                [ Card.Header.title [] [ str "Write a draft!" ] ]
                              Card.content []
                                [ Input.text [ Input.Placeholder "Your draft"
                                               Input.Value model.DraftForm.Title
                                               Input.OnChange (fun ev -> UpdateDraftForm (ev.Value, "title") |> dispatch)
                                               Input.Option.Props
                                                 [ OnKeyUp (fun key -> if key.which = 13.0 then dispatch CreateDraft)  ] ]
                                  Input.text [ Input.Placeholder "Decription"
                                               Input.Value model.DraftForm.Description
                                               Input.OnChange (fun ev -> UpdateDraftForm (ev.Value, "description") |> dispatch)
                                               Input.Option.Props
                                                 [ OnKeyUp (fun key -> if key.which = 13.0 then dispatch CreateDraft)  ] ] ]
                              Card.footer []
                                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> dispatch CreateDraft) ] ]
                                    [ str "Submit" ] ] ] ] ]
                  yield! model.Drafts |> toCardRows dispatch ] ] ]

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
