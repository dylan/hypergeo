module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Round exposing (..)


factorial : Float -> Float
factorial n =
    case n of
        0 ->
            1

        _ ->
            n * factorial (n - 1)


combine : Float -> Float -> Float
combine x y =
    factorial x / (factorial y * factorial (x - y))


{-| Given hitsInPopulation and a populationSize. If you have a drawSize to start,
what are the odds you'll have desiredHits in that draw.
-}
hypergeometricDistribution : Float -> Float -> Float -> Float -> Float
hypergeometricDistribution hitsInPopulation populationSize drawSize desiredHits =
    combine hitsInPopulation desiredHits
        * combine (populationSize - hitsInPopulation) (drawSize - desiredHits)
        / combine populationSize drawSize


type alias Model =
    { deckSize : Float
    , handSize : Float
    , hitSize : Float
    , successCount : Float
    , result : Float
    }


model : Model
model =
    Model 4 60 7 1 0


type Msg
    = DeckSize String
    | HandSize String
    | HitSize String
    | SuccessCount String
    | Calculate


cast : String -> Float
cast string =
    case String.toFloat string of
        Err value ->
            0.0

        Ok value ->
            value


update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate ->
            { model | result = calculate model }

        DeckSize value ->
            { model | deckSize = cast value }

        HandSize value ->
            { model | handSize = cast value }

        HitSize value ->
            { model | hitSize = cast value }

        SuccessCount value ->
            { model | successCount = cast value }


calculate : Model -> Float
calculate model =
    (hypergeometricDistribution model.hitSize model.deckSize model.handSize model.successCount) * 100


view : Model -> Html Msg
view model =
    div []
        [ fieldset []
            [ li [] [ text "Deck size:", input [ placeholder "60", onInput DeckSize ] [] ]
            , li [] [ text "Opening hand size:", input [ placeholder "7", onInput HandSize ] [] ]
            , li [] [ text "Desired cards in deck:", input [ placeholder "4", onInput HitSize ] [] ]
            , li [] [ text "Number of desired cards wanted in opening hand:", input [ placeholder "1", onInput SuccessCount ] [] ]
            , button [ onClick Calculate ] [ text "Calculate" ]
            , text <| (Round.ceiling 2 model.result) ++ "%"
            ]
        ]


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
