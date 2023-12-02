module Main exposing (main)

import Array
import Browser exposing (Document)
import Dict exposing (Dict, update)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import List.Extra exposing (groupsOf)
import Random


type alias Card =
    { name : String
    , imageUrl : String
    , faceUp : Bool
    }


type alias Model =
    { cards : Dict String Card
    , selectedCard : Maybe Card
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { cards =
            List.map
                (\person -> ( person.name, person ))
                allCards
                |> Dict.fromList
      , selectedCard = Nothing
      }
    , Cmd.none
    )


type Msg
    = PickCard
    | ResetBoard Int
    | CardClicked String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickCard ->
            ( model, Random.generate ResetBoard (Random.int 0 23) )

        ResetBoard idx ->
            ( { cards =
                    Dict.map
                        (\_ card -> { card | faceUp = True })
                        model.cards
              , selectedCard = Array.fromList allCards |> Array.get idx
              }
            , Cmd.none
            )

        CardClicked name ->
            ( { model
                | cards =
                    Dict.update
                        name
                        (Maybe.map (\card -> { card | faceUp = not card.faceUp }))
                        model.cards
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Guess Who"
    , body =
        [ layout
            []
            (row
                [ width fill, height fill, centerY ]
                [ column [ width (fill |> minimum 300), height fill ] [ viewCardPickerSection model ]
                , column
                    [ width (fillPortion 4), height fill, spacingXY 24 24 ]
                    (viewCardRows model)
                ]
            )
        ]
    }


viewCard : Bool -> Card -> Element Msg
viewCard clickable card =
    button []
        { onPress =
            if clickable then
                Just (CardClicked card.name)

            else
                Just NoOp
        , label =
            if card.faceUp then
                image
                    [ height (px 150), width (px 130) ]
                    { src = card.imageUrl, description = card.name }

            else
                el
                    [ height (px 150)
                    , width (px 130)
                    , Background.color (rgb255 28 68 124)
                    ]
                    (el
                        [ centerX
                        , centerY
                        , Font.bold
                        , Font.size 128
                        , Font.color (rgb255 193 53 56)
                        ]
                        (text "?")
                    )
        }


viewCardPickerSection : Model -> Element Msg
viewCardPickerSection model =
    case model.selectedCard of
        Just card ->
            column [ centerX, centerY, spacingXY 0 24 ]
                [ viewChosenCard card
                , button [ centerX ]
                    { label = text "New Game"
                    , onPress = Just PickCard
                    }
                ]

        Nothing ->
            el [ centerX, centerY, spacingXY 0 24 ]
                (button [ centerX ]
                    { label = text "Choose Card"
                    , onPress = Just PickCard
                    }
                )


viewChosenCard : Card -> Element Msg
viewChosenCard card =
    viewCard True card


viewCardRows : Model -> List (Element Msg)
viewCardRows model =
    Dict.values model.cards
        |> groupsOf 6
        |> List.map
            (\cards ->
                el [ centerX, centerY ] (row [ spacingXY 24 24 ] (List.map (viewCard True) cards))
            )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


allCards : List Card
allCards =
    [ { name = "alex", imageUrl = "./assets/images/alex.png", faceUp = True }
    , { name = "alfred", imageUrl = "./assets/images/alfred.png", faceUp = True }
    , { name = "anita", imageUrl = "./assets/images/anita.png", faceUp = True }
    , { name = "anne", imageUrl = "./assets/images/anne.png", faceUp = True }
    , { name = "bernard", imageUrl = "./assets/images/bernard.png", faceUp = True }
    , { name = "bill", imageUrl = "./assets/images/bill.png", faceUp = True }
    , { name = "charles", imageUrl = "./assets/images/charles.png", faceUp = True }
    , { name = "claire", imageUrl = "./assets/images/claire.png", faceUp = True }
    , { name = "david", imageUrl = "./assets/images/david.png", faceUp = True }
    , { name = "eric", imageUrl = "./assets/images/eric.png", faceUp = True }
    , { name = "frans", imageUrl = "./assets/images/frans.png", faceUp = True }
    , { name = "george", imageUrl = "./assets/images/george.png", faceUp = True }
    , { name = "herman", imageUrl = "./assets/images/herman.png", faceUp = True }
    , { name = "joe", imageUrl = "./assets/images/joe.png", faceUp = True }
    , { name = "maria", imageUrl = "./assets/images/maria.png", faceUp = True }
    , { name = "max", imageUrl = "./assets/images/max.png", faceUp = True }
    , { name = "paul", imageUrl = "./assets/images/paul.png", faceUp = True }
    , { name = "peter", imageUrl = "./assets/images/peter.png", faceUp = True }
    , { name = "philip", imageUrl = "./assets/images/philip.png", faceUp = True }
    , { name = "richard", imageUrl = "./assets/images/richard.png", faceUp = True }
    , { name = "robert", imageUrl = "./assets/images/robert.png", faceUp = True }
    , { name = "sam", imageUrl = "./assets/images/sam.png", faceUp = True }
    , { name = "susan", imageUrl = "./assets/images/susan.png", faceUp = True }
    , { name = "tom", imageUrl = "./assets/images/tom.png", faceUp = True }
    ]
