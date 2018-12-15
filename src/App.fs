module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Browser = Fable.Import.Browser

// MODEL

type Draft =
    | NewDraft of string
    | BumpedDraft of string * int
    | RejectedDraft of string

type Model =
    { DraftForm : string
      Drafts : Draft list }

type Msg =
| UpdateDraftForm of string
| CreateDraft
| BumpDraft of string
| RejectDraft of string
| UnbumpDraft of string
(* _3_a_ define the UnbumpDraft message here. *)

let init() : Model =
    { DraftForm = ""
      Drafts = [] }

// UPDATE

let bump (title : string) (d : Draft) =
    match d with
    | NewDraft t ->
        if t = title then
            sprintf "Draft %s has its first bump!" t
            |> Browser.console.log
            (BumpedDraft (t, 1))
        else d
    | BumpedDraft (t, b) ->
        if t = title then
            sprintf "Draft %s has now %d bumps!" t b
            |> Browser.console.log
            (BumpedDraft (t, b + 1))
        else d
    | RejectedDraft _ -> d

let unbump (title : string) (d : Draft) =
    match d with
    | NewDraft _ -> d
    | BumpedDraft (t, b) when b > 1 ->
        if t = title then
            (BumpedDraft (t, b - 1))
        else d
    | BumpedDraft (t, _) ->
        NewDraft t
    | RejectedDraft _ -> d

let reject (title : string) (d : Draft) =
    match d with
    | NewDraft t ->
        if t = title then (RejectedDraft t) else d
    | BumpedDraft _ -> d
    | RejectedDraft _ -> d

let update (msg:Msg) (model:Model) =
    match msg with
    | UpdateDraftForm content ->
        { model with DraftForm = content }
    | CreateDraft ->
        let newDraft = NewDraft model.DraftForm
        { model with
            DraftForm = ""
            Drafts = newDraft::model.Drafts }
    | BumpDraft title ->
        let drafts =
            model.Drafts
            |> List.map (bump title)
        { model with Drafts = drafts }
    | RejectDraft title ->
        let drafts = 
            model.Drafts
            |> List.map (reject title)
        { model with Drafts = drafts }
    | UnbumpDraft title ->
        let drafts =
            model.Drafts
            |> List.map (unbump title)
        { model with Drafts = drafts }
    (* _3_b_ Handle the UnbumpDraft message here - use the unbump method *)

// VIEW (rendered with React)

open Fulma

let newDraftTile dispatch (title : string) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str title ] ]
              Card.content []
                [ Content.content [] [ str "Your prestine card draft." ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> BumpDraft title |> dispatch) ] ]
                    [ str "Bump" ]
                  Card.Footer.a [ (*_1_*)(GenericOption.Props [ OnClick (fun _ -> RejectDraft title |> dispatch) ]) ]
                    [ str "Reject" ] ] ] ]

let rejectedDraftTile dispatch (title : string) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str title ] ]
              Card.content []
                [ Content.content [] [ str "Unfortunately this draft has been rejected 🙁" ] ]
              Card.footer []
                [ ] ] ]

let bumpedDraftTile dispatch (title : string) (bumps : int) =
    (*
        _2_ This function just reuses the tile we create for new drafts.
        Can you create a similar tile that shows you how many bumps the draft
        received in its content? Also: give it a link to bump it even more in the footer!
    *)
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str title ] ]
              Card.content []
                [ Content.content [] [ str (sprintf "This draft was bumped %d times." bumps) ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> BumpDraft title |> dispatch) ] ]
                    [ str "Bump more!" ]
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> UnbumpDraft title |> dispatch) ] ]
                    [ str "Unbump" ] ] ] ]

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
                                               Input.Value model.DraftForm
                                               Input.OnChange (fun ev -> UpdateDraftForm ev.Value |> dispatch)
                                               Input.Option.Props
                                                 [ (* _4_ there is a OnKeyUp you can use here
                                                      the event holds the id of the pressed key
                                                      you can look up which id corresponds with 'enter'
                                                    *) ] ] ]
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
