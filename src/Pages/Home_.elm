module Pages.Home_ exposing (Model, Msg, page)

import Components.Layout as Layout exposing (initLayout)
import Components.Svg as SVG exposing (Logo(..))
import Dict exposing (Dict)
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (Html, a, b, button, div, h1, h2, h5, input, label, li, p, section, text, textarea, ul)
import Html.Attributes exposing (checked, class, classList, href, id, rel, tabindex, target, type_, value)
import Html.Attributes.Aria exposing (ariaLabel, ariaLabelledby)
import Html.Events exposing (onCheck, onClick, onInput)
import Page
import Request
import Shared
import Svg exposing (desc)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ _ =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    { items : Dict Int Item
    , id : Int
    , editId : Maybe Int
    , error : Maybe String
    }


type alias Item =
    { check : Bool
    , name : String
    , description : String
    }


type alias Error =
    { name : String
    , desc : String
    }


init : Model
init =
    { items = Dict.empty
    , id = 0
    , editId = Nothing
    , error = Nothing
    }


initItem : Item
initItem =
    { check = False
    , name = ""
    , description = ""
    }



-- UPDATE


type Msg
    = AddItem Item
    | Check Int Bool
    | InputName Int String
    | InputDesc Int String
    | Remove Int
    | Edit Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddItem item_ ->
            let
                cleanItem =
                    Dict.get model.id model.items
                        |> Maybe.withDefault initItem

                insetNew =
                    if String.isEmpty cleanItem.name then
                        { model | error = Just "Name is required" }

                    else if String.isEmpty cleanItem.description then
                        { model | error = Just "Description is required" }

                    else
                        { model
                            | id = model.id + 1
                            , items =
                                Dict.insert model.id
                                    { check = item_.check
                                    , name = item_.name
                                    , description = item_.description
                                    }
                                    model.items
                            , error = Nothing
                        }
            in
            insetNew

        Check id_ check ->
            let
                item_ =
                    Dict.get id_ model.items
                        |> Maybe.withDefault initItem

                updateName =
                    \_ ->
                        Just
                            { item_
                                | check = check
                                , name = item_.name
                                , description = item_.description
                            }
            in
            { model | items = Dict.update id_ updateName model.items }

        InputName id_ name ->
            let
                item_ =
                    Dict.get id_ model.items
                        |> Maybe.withDefault initItem

                updateName =
                    \_ ->
                        Just
                            { item_
                                | check = item_.check
                                , name = name
                                , description = item_.description
                            }
            in
            { model | items = Dict.update id_ updateName model.items }

        InputDesc id_ desc ->
            let
                item_ =
                    Dict.get id_ model.items
                        |> Maybe.withDefault initItem

                updateName =
                    \_ ->
                        Just
                            { item_
                                | check = item_.check
                                , name = item_.name
                                , description = desc
                            }
            in
            { model | items = Dict.update id_ updateName model.items }

        Remove id_ ->
            { model | items = Dict.remove id_ model.items }

        Edit id_ ->
            { model | editId = Just id_ }



-- VIEW


view : Model -> View Msg
view model =
    { title = "Revex - Home"
    , body =
        Layout.viewLayout
            { initLayout
                | route = Route.Home_
                , mainAttrs = []
                , mainContent = [ viewPage model ]
            }
    }


viewPage : Model -> Html Msg
viewPage model =
    let
        items =
            Dict.get model.id model.items
                |> Maybe.withDefault initItem
    in
    section [ class "block" ]
        [ List.map
            (\i ->
                let
                    item_ =
                        Dict.get i model.items
                            |> Maybe.withDefault initItem

                    viewLi =
                        li [ classList [ ( "item", True ), ( "item--check", item_.check ) ] ]

                    view_ =
                        [ label
                            [ classList
                                [ ( "checkbox", True )
                                , ( "checkbox--check", item_.check )
                                ]
                            ]
                            [ input [ type_ "checkbox", checked item_.check, onCheck <| Check i ] []
                            ]
                        , b [ class "name" ] [ text item_.name ]
                        , p [ class "desc" ] [ text item_.description ]
                        , button [ class "remove", onClick <| Remove i ] [ text "Remove" ]
                        , button [ class "edit", onClick <| Edit i ] [ text "edit" ]
                        ]
                in
                if item_ == initItem then
                    text ""

                else if Maybe.withDefault -1 model.editId == i then
                    viewLi <|
                        view_
                            ++ [ div [ class "add" ]
                                    [ input [ onInput <| InputName i, value item_.name ] []
                                    , input [ onInput <| InputDesc i, value item_.description ] []
                                    , p [ classList [ ( "error", True ), ( "hidden", model.error == Nothing ) ] ]
                                        [ text <| Maybe.withDefault "" model.error ]
                                    ]
                               ]

                else
                    viewLi
                        view_
            )
            (List.range 0 (model.id - 1))
            -- |> List.concat
            |> ul [ class "ul" ]
        , div [ class "add" ]
            [ input [ onInput <| InputName model.id ] []
            , input [ onInput <| InputDesc model.id ] []
            , button [ class "p-3 bg-surface-300", onClick <| AddItem items ] [ text "+" ]
            , p [ classList [ ( "error", True ), ( "hidden", model.error == Nothing ) ] ]
                [ text <| Maybe.withDefault "" model.error ]
            ]
        ]
