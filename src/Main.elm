module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Debug exposing (log)
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (class, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as Liste
import Task
import Time



-- MAIN
{- This is where we define the type of elm application we want.
   In this case we're using Browser.element, which just means a
   basic Elm app without any side effects.
-}


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL
-- Let's define the relevant data structures we need in our application
{- this data structure will hold the time that we need to figure out
   when the next game should start
-}


type alias TimeData =
    { zone : Time.Zone
    , time : Time.Posix
    }



{- Data Structures for word list.
   Here we're using an in-memory wordlist to mimic an external list.
   Each word has an associated piece of data which lets us know if it's
   Already been used or not.
-}


type WordStatus
    = Used
    | Available


type alias WordState =
    ( String, WordStatus )



-- Data structure for game state


type GameEnd
    = Won
    | Lost


type Colour
    = Green
    | Yellow
    | Grey


type alias Guess =
    List Char


type alias GameData =
    { currentWord : String
    , guessList : List Guess
    }


type GameState
    = Active GameData String
    | FinishedGame GameData GameEnd



-- Final Model


type alias Model =
    { timeInfo : TimeData, game : GameState, wordList : List WordState }



-- Session is Model plus effects


type alias Session =
    ( Model, Cmd Msg )



-- initial State
-- the initial data we need since we dont have a persistent backend


initialWordList : List WordState
initialWordList =
    [ ( "testing", Used )
    , ( "coffee", Used )
    , ( "virtual", Used )
    , ( "breakout", Used )
    , ( "effort", Used )
    , ( "peanuts", Used )
    , ( "parachute", Available )
    ]



-- the initial state of our app


initialModel : Model
initialModel =
    { timeInfo = TimeData Time.utc (Time.millisToPosix 0)
    , game = Active { currentWord = "started", guessList = [] } ""
    , wordList = initialWordList
    }



-- the initial effects we wants to happen at app start
-- essentially just getting the correct time and timezone


initialEffects : List (Cmd Msg)
initialEffects =
    [ Task.perform AdjustTimeZone Time.here
    , Task.perform Tick Time.now
    ]


init : () -> Session
init _ =
    ( initialModel, Cmd.batch initialEffects )
    

-- UPDATE
-- the messages that can be sent to our application


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | InputUpdated String
    | SubmitGuessClicked


update : Msg -> Model -> Session
update msg ({ timeInfo, game, wordList } as model) =
    case msg of
        Tick newTime ->
            { model | timeInfo = { timeInfo | time = newTime } }
                |> timeTransition
                |> withNoEffects

        AdjustTimeZone newZone ->
            { model | timeInfo = { timeInfo | zone = newZone } }
                |> withNoEffects

        InputUpdated updatedGuess ->
            case game of
                Active gameData _ ->
                    { model | game = Active gameData updatedGuess }
                        |> withNoEffects

                _ ->
                    model |> withNoEffects

        SubmitGuessClicked ->
            case game of
                Active gameData currentGuess ->
                    let
                        updatedGame =
                            winTransition (updateGuesses gameData currentGuess)
                    in
                    { model | game = updatedGame } |> withNoEffects

                _ ->
                    model |> withNoEffects



-- UPDATE HELPERS
-- handles time-based game state transitions


timeTransition : Model -> Model
timeTransition ({ timeInfo, game, wordList } as model) =
    if isTimeToStart timeInfo then
        let
            ( nextWord, updatedList ) =
                nextAvailableWord wordList

            newGameState =
                nextWord
                    |> Maybe.map (\word -> Active (GameData word []) "")
                    |> Maybe.withDefault game
        in
        Model timeInfo newGameState updatedList

    else
        model


updateGuesses : GameData -> String -> GameData
updateGuesses ({ guessList } as gameData) activeGuess =
    { gameData | guessList = guessList ++ [ String.toList activeGuess ] }


winTransition : GameData -> GameState
winTransition ({ currentWord, guessList } as gameData) =
    let
        foundWin =
            guessList
                |> (List.reverse >> List.head)
                |> Maybe.map (guessToWord >> (==) currentWord)
                |> Maybe.withDefault False
    in
    if foundWin then
        FinishedGame gameData Won

    else if List.length guessList == 6 then
        FinishedGame gameData Lost

    else
        Active gameData ""


isTimeToStart : TimeData -> Bool
isTimeToStart { time, zone } =
    Time.toSecond zone time == 0



-- && modBy 5 (Time.toMinute zone time) == 0


nextAvailableWord : List WordState -> ( Maybe String, List WordState )
nextAvailableWord wordList =
    let
        maybeWord =
            Liste.find (\( _, status ) -> status == Available) wordList
    in
    case maybeWord of
        Nothing ->
            let 
                newList = List.map (\(word, _) -> (word, Available)) wordList
            in
            nextAvailableWord newList

        Just ( nextWord, _ ) ->
            let
                newList =
                    Liste.setIf (\( word, _ ) -> word == nextWord) ( nextWord, Used ) wordList
            in
            ( Just nextWord, newList )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW
-- We split our view into various subfunctions so nothing gets too complicated


view : Model -> Html Msg
view model =
    div
        [ style "height" "100vh"
        , style "width" "100vw"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "flex-start"
        , style "align-items" "center"
        , style "text-align" "center"
        ]
        [ div
            [ style "font-size" "3em"
            , style "min-height" "20vh"
            , style "display" "flex"
            , style "align-items" "center"
            ]
            [ text "Wordelm" ]
        , div [] [ text "A new game starts every minute!" ]
        , viewTime model.timeInfo
        , viewGame model.game
        ]


viewTime : TimeData -> Html Msg
viewTime current =
    let
        timeComponentToString func =
            func current.zone current.time
                |> String.fromInt
                |> atLeastTwo

        hour =
            timeComponentToString Time.toHour

        minute =
            timeComponentToString Time.toMinute

        second =
            timeComponentToString Time.toSecond

        fullTime =
            hour ++ " : " ++ minute ++ " : " ++ second
    in
    div [] [ text fullTime ]


viewGame : GameState -> Html Msg
viewGame gamestate =
    let
        styledDiv =
            div
                [ style "height" "100%"
                , style "display" "flex"
                , style "flex-direction" "column"
                , style "justify-content" "center"
                ]
    in
    case gamestate of
        Active gameData guess ->
            styledDiv [ viewActiveGame gameData guess ]

        FinishedGame gameData result ->
            let
                guessElms =
                    div [] <| List.map (viewGuess gameData.currentWord) gameData.guessList

                resultText =
                    if result == Won then
                        "won"

                    else
                        "lost"
            in
            styledDiv [ guessElms, text <| "The game is over. You " ++ resultText ++ "!" ]


viewActiveGame : GameData -> String -> Html Msg
viewActiveGame { currentWord, guessList } activeGuess =
    let
        guessElms =
            div [] <| List.map (viewGuess currentWord) guessList

        guessbox =
            div [] [ input [ placeholder "enter guess here", value activeGuess, onInput InputUpdated ] [] ]

        submit =
            div [] [ button [ onClick SubmitGuessClicked ] [ text "submit guess" ] ]

        numberOfLetters =
            div []
                [ text "The length of the word is: "
                , text <| String.fromInt <| String.length currentWord
                ]
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "space-around"
        , style "align-items" "center"
        , style "min-height" "40vh"
        , style "min-width" "80vw"
        ]
        [ guessElms, guessbox, numberOfLetters, submit ]


viewGuess : String -> Guess -> Html Msg
viewGuess currentWord guess =
    let
        ( firstPassWord, firstPassGuess ) =
            String.toList currentWord
                |> List.map2
                    (\guessLetter currentLetter ->
                        if guessLetter == currentLetter then
                            ( Nothing, ( guessLetter, Just Green ) )

                        else
                            ( Just currentLetter, ( guessLetter, Nothing ) )
                    )
                    guess
                |> List.unzip

        ( _, colourMapped ) =
            Liste.mapAccuml analyzer (List.filterMap identity firstPassWord) firstPassGuess
    in
    colourMapped
        |> List.map
            (\( guessLetter, colour ) ->
                span
                    [ style "background-color" (colourToString colour) ]
                    [ (String.fromChar >> String.toUpper >> text) guessLetter ]
            )
        |> div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "width" "30vh"
            , style "font-size" "20px"
            , style "font-weight" "bold"
            ]



-- (a -> b -> ( a, c )) -> a -> List b -> ( a, List c )


analyzer : List Char -> ( Char, Maybe Colour ) -> ( List Char, ( Char, Colour ) )
analyzer remainingChars ( guessChar, colour ) =
    if colour == Just Green then
        ( remainingChars, ( guessChar, Green ) )

    else if List.member guessChar remainingChars then
        ( Liste.remove guessChar remainingChars, ( guessChar, Yellow ) )

    else
        ( remainingChars, ( guessChar, Grey ) )



-- VIEW HELPERS
-- HELPERS
-- ensures that single digit numbers are represented with a leading zero
-- so "04" vs "4". It just looks neater.


atLeastTwo : String -> String
atLeastTwo string =
    if String.length string == 2 then
        string

    else
        "0" ++ string


guessToWord : Guess -> String
guessToWord guess =
    List.map String.fromChar guess
        |> String.concat


colourToString : Colour -> String
colourToString colour =
    case colour of
        Green ->
            "green"

        Yellow ->
            "yellow"

        Grey ->
            "gray"



-- convenient functions for turning our models into full sessions


withNoEffects : Model -> Session
withNoEffects model =
    ( model, Cmd.none )
