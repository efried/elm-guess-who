module Main exposing (main)

import Array
import Browser exposing (Document)
import Dict exposing (Dict, update)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (time)
import List.Extra exposing (cycle, groupsOf, last)
import Random
import Task
import Time exposing (toMinute, utc)


type alias Card =
    { name : String
    , imageUrl : String
    , faceUp : Bool
    }


type alias Model =
    { cards : Dict String Card
    , selectedCard : Maybe Card
    , secretWord : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { cards =
            List.map
                (\person -> ( person.name, person ))
                allCards
                |> Dict.fromList
      , selectedCard = Nothing
      , secretWord = Nothing
      }
    , Cmd.none
    )


type Msg
    = PickCard
    | ResetBoard Int
    | GetSecretWord Int Time.Posix
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
              , secretWord = Nothing
              }
            , Task.perform (GetSecretWord idx) Time.now
            )

        GetSecretWord idx time ->
            ( { model
                | secretWord = getSecretWord idx time
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
            [ Font.family [ Font.typeface "Fredoka", Font.sansSerif ] ]
            (row
                [ width fill, height fill, centerY ]
                [ column
                    [ width (fill |> minimum 300)
                    , height fill
                    , centerY
                    , Background.color (rgb255 205 41 44)
                    , Border.widthEach { left = 0, right = 2, bottom = 0, top = 0 }
                    , spacingXY 0 24
                    ]
                    [ column [ centerX, centerY, spacingXY 0 48 ] [ logo, viewCardPickerSection model ] ]
                , column
                    [ Background.color (rgb255 28 68 124), width (fillPortion 4), height fill, spacingXY 24 24 ]
                    (viewCardRows model)
                ]
            )
        ]
    }


logo : Element Msg
logo =
    row [ centerX ]
        [ column []
            [ el
                [ alignRight
                , Font.size 48
                , Font.extraBold
                , Font.color (rgb255 255 255 255)
                ]
                (text "Guess")
            , el
                [ alignRight
                , Font.size 48
                , Font.extraBold
                , Font.color (rgb255 255 255 255)
                ]
                (text "Who")
            ]
        , column []
            [ el
                [ centerX
                , centerY
                , Font.size 96
                , Font.extraBold
                , Font.color (rgb255 255 255 255)
                ]
                (text "?")
            ]
        ]


viewCard : Bool -> Card -> Element Msg
viewCard clickable card =
    button [ centerX ]
        { onPress =
            if clickable then
                Just (CardClicked card.name)

            else
                Just NoOp
        , label =
            if card.faceUp then
                image
                    [ height (px 150), width (px 130), Border.rounded 6, clip ]
                    { src = card.imageUrl, description = card.name }

            else
                el
                    [ height (px 150)
                    , width (px 130)
                    , Background.color (rgb255 205 41 44)
                    , Border.rounded 6
                    ]
                    (el
                        [ centerX
                        , centerY
                        , Font.bold
                        , Font.size 128
                        , Font.color (rgb255 255 255 255)
                        ]
                        (text "?")
                    )
        }


viewCardPickerSection : Model -> Element Msg
viewCardPickerSection model =
    case model.selectedCard of
        Just card ->
            column [ centerX, centerY, spacingXY 0 24 ]
                [ viewCard False card
                , el
                    [ centerX
                    , Font.color (rgb255 255 255 255)
                    ]
                    (text ("Secret Word: " ++ Maybe.withDefault "" model.secretWord))
                , button [ centerX ]
                    { label =
                        el
                            [ Background.color (rgb255 28 68 124)
                            , Font.color (rgb255 255 255 255)
                            , paddingXY 16 8
                            , Border.rounded 6
                            ]
                            (text "New Game")
                    , onPress = Just PickCard
                    }
                ]

        Nothing ->
            el [ centerX, centerY, spacingXY 0 24 ]
                (button [ centerX ]
                    { label =
                        el
                            [ Background.color (rgb255 28 68 124)
                            , Font.color (rgb255 255 255 255)
                            , paddingXY 16 8
                            , Border.rounded 6
                            ]
                            (text "Choose a Card")
                    , onPress = Just PickCard
                    }
                )


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
    [ { name = "alex", imageUrl = "./assets/images/alex.avif", faceUp = True }
    , { name = "alfred", imageUrl = "./assets/images/alfred.avif", faceUp = True }
    , { name = "anita", imageUrl = "./assets/images/anita.avif", faceUp = True }
    , { name = "anne", imageUrl = "./assets/images/anne.avif", faceUp = True }
    , { name = "bernard", imageUrl = "./assets/images/bernard.avif", faceUp = True }
    , { name = "bill", imageUrl = "./assets/images/bill.avif", faceUp = True }
    , { name = "charles", imageUrl = "./assets/images/charles.avif", faceUp = True }
    , { name = "claire", imageUrl = "./assets/images/claire.avif", faceUp = True }
    , { name = "david", imageUrl = "./assets/images/david.avif", faceUp = True }
    , { name = "eric", imageUrl = "./assets/images/eric.avif", faceUp = True }
    , { name = "frans", imageUrl = "./assets/images/frans.avif", faceUp = True }
    , { name = "george", imageUrl = "./assets/images/george.avif", faceUp = True }
    , { name = "herman", imageUrl = "./assets/images/herman.avif", faceUp = True }
    , { name = "joe", imageUrl = "./assets/images/joe.avif", faceUp = True }
    , { name = "maria", imageUrl = "./assets/images/maria.avif", faceUp = True }
    , { name = "max", imageUrl = "./assets/images/max.avif", faceUp = True }
    , { name = "paul", imageUrl = "./assets/images/paul.avif", faceUp = True }
    , { name = "peter", imageUrl = "./assets/images/peter.avif", faceUp = True }
    , { name = "philip", imageUrl = "./assets/images/philip.avif", faceUp = True }
    , { name = "richard", imageUrl = "./assets/images/richard.avif", faceUp = True }
    , { name = "robert", imageUrl = "./assets/images/robert.avif", faceUp = True }
    , { name = "sam", imageUrl = "./assets/images/sam.avif", faceUp = True }
    , { name = "susan", imageUrl = "./assets/images/susan.avif", faceUp = True }
    , { name = "tom", imageUrl = "./assets/images/tom.avif", faceUp = True }
    ]


secretWords : List String
secretWords =
    [ "throat"
    , "mainstream"
    , "convince"
    , "basin"
    , "permission"
    , "plant"
    , "straighten"
    , "dirty"
    , "year"
    , "customer"
    , "brag"
    , "half"
    , "carbon"
    , "he"
    , "dialect"
    , "strength"
    , "pair"
    , "prefer"
    , "breakdown"
    , "intermediate"
    , "elect"
    , "attraction"
    , "day"
    , "transition"
    ]


getSecretWord : Int -> Time.Posix -> Maybe String
getSecretWord cardIndex time =
    let
        minute : Int
        minute =
            toMinute utc time

        secretWordIndex : Int
        secretWordIndex =
            (minute // 3) + cardIndex
    in
    cycle secretWordIndex secretWords |> last
