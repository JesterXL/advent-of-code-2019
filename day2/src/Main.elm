{- 
    Jesse Warden
    jesterxl@jessewarden.com
    jesse.warden@gmail.com
    Twitter: twitter.com/jesterxl
    YouTube: youtube.com/jessewarden
    https://adventofcode.com/2019/day/2
-}

module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, b, table, tr, th, td)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

import Material.Button exposing (textButton, buttonConfig)
import Material.Card as Card exposing (CardActions, CardBlock, card, cardActionButton, cardActionIcon, cardActions, cardBlock, cardConfig, cardMedia, cardMediaConfig, cardPrimaryAction, cardPrimaryActionConfig)
import Material.Typography as Typography exposing (headline5, subtitle2, caption, subtitle1, subtitle2, caption, button, overline)
import Material.Theme as Theme
import Material.TextArea exposing (textArea, textAreaConfig)
import Material.Elevation exposing (z4)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, usLocale)

import Debug exposing (log)
import String exposing (split)
import Chunk exposing (chunk)
import Array exposing (Array)

type alias Model =
    { initialInput : String
    , intCodes : List Int
    , processedIntCodes : List Int
    , noun : Int
    , verb : Int
    , targetNounVerbProgramResult : Maybe NounVerbProgramResult
    , challenge2Answer : Int }

initialModel : Model
initialModel =
    { initialInput = ""
    , intCodes = []
    , processedIntCodes = []
    , noun = 0
    , verb = 0 
    , targetNounVerbProgramResult = Nothing
    , challenge2Answer = 0}

type Algorithm
    = AddCode
    | MultiplyCode
    | StopCode

type alias NounVerbProgramResult =
    { noun : Int
    , verb : Int 
    , firstPosition : Int }

init : Int -> ( Model, Cmd Msg)
init seed =
    ( initialModel, Cmd.none )

type Msg
    = LoadDefaultInput
    | ParseInput
    | DetermineNounAndVerb

update : Msg -> Model -> (Model, Cmd Msg )
update msg model =
    case msg of
        LoadDefaultInput ->
           ( { model | initialInput = puzzleInput }, Cmd.none)
        ParseInput ->
            let
                -- Challenge 1
                -- Guess #1: 3654868

                -- Challenge 2
                -- find 19690720



                intList = parsePuzzleInput model.initialInput
                preProcessedIntList = preProcessOptCodes intList
                processedList = runProgram preProcessedIntList
                positionZero =
                    Array.fromList processedList
                    |> Array.get 0
                    |> Maybe.withDefault 0
                msg1 = log "positionZero:" positionZero


            in
            ( { model | intCodes = intList, processedIntCodes = processedList }, Cmd.none )
        DetermineNounAndVerb ->
            let
                -- 19690720

                -- preProcessedIntList = setNounAndVerbInMemory 0 0 model.intCodes
                -- processedList = runProgram preProcessedIntList
                -- positionZero =
                --     Array.fromList processedList
                --     |> Array.get 0
                --     |> Maybe.withDefault 0
                -- msg1 = log "getRangeTuples" (getRangeTuples 0)
                -- msg2 = log "getAllRangedTuples" getAllRangedTuples
                targetNounVerbProgramResult =
                    getAllRangedTuples
                    |> List.foldl (\tupleList acc ->
                                        acc ++ tryManyNounsAndVerbs model.intCodes tupleList) []
                    |> List.filter (\result -> 
                                        result.firstPosition == 19690720)
                    |> List.head
                challenge2Answer =
                    case targetNounVerbProgramResult of
                        Nothing ->
                            0
                        Just target ->
                            100 * target.noun + target.verb
            in
            ( { model 
                | targetNounVerbProgramResult = targetNounVerbProgramResult
                , challenge2Answer = challenge2Answer }, Cmd.none)

tryManyNounsAndVerbs : List Int -> List (Int, Int) -> List NounVerbProgramResult
tryManyNounsAndVerbs intCodes listOfTuples =
    List.foldl (\(noun, verb) acc ->
        let
            processedMemory =
                setNounAndVerbInMemory noun verb intCodes
            processedList = runProgram processedMemory
            positionZero =
                Array.fromList processedList
                |> Array.get 0
                |> Maybe.withDefault 0
        in
        acc ++ [NounVerbProgramResult noun verb positionZero]) [] listOfTuples
    

parsePuzzleInput : String -> List Int
parsePuzzleInput string =
    String.split "," string
    |> List.map String.toInt
    |> List.map (\m -> case m of
                    Just val -> val
                    Nothing -> 0)

detectAlgorithmFromRow : List Int -> Algorithm
detectAlgorithmFromRow row =
    let
        firstItemMaybe = List.head row
    in
    case firstItemMaybe of
        Just val ->
            if val == 1 then
                AddCode
            else if val == 2 then
                MultiplyCode
            else if val == 99 then
                StopCode
            else
                StopCode
        Nothing ->
            StopCode

processAddCode : List Int -> List Int -> List Int 
processAddCode rowList opCodes =
    let
        row = Array.fromList rowList
        opCodeArray = Array.fromList opCodes
        firstPosition = Array.get 1 row |> Maybe.withDefault 0
        secondPosition = Array.get 2 row |> Maybe.withDefault 0
        thirdPosition = Array.get 3 row |> Maybe.withDefault 0
        value1 = Array.get firstPosition opCodeArray |> Maybe.withDefault 0
        value2 = Array.get secondPosition opCodeArray |> Maybe.withDefault 0
        finalValue = value1 + value2
        updatedOpCodesArray = Array.set thirdPosition finalValue opCodeArray
    in
    Array.toList updatedOpCodesArray

processMultiplyCode : List Int -> List Int -> List Int 
processMultiplyCode rowList opCodes =
    let
        row = Array.fromList rowList
        opCodeArray = Array.fromList opCodes
        firstPosition = Array.get 1 row |> Maybe.withDefault 0
        secondPosition = Array.get 2 row |> Maybe.withDefault 0
        thirdPosition = Array.get 3 row |> Maybe.withDefault 0
        value1 = Array.get firstPosition opCodeArray |> Maybe.withDefault 0
        value2 = Array.get secondPosition opCodeArray |> Maybe.withDefault 0
        finalValue = value1 * value2
        updatedOpCodesArray = Array.set thirdPosition finalValue opCodeArray
    in
    Array.toList updatedOpCodesArray

runProgram : List Int -> List Int
runProgram intList =
    let
        chunked = chunk 4 intList
    in
    List.foldl (\opt acc ->
        let
            algo = detectAlgorithmFromRow opt
            -- msg0 = log "acc" acc
            processed =
                case algo of
                    AddCode ->
                        processAddCode opt acc
                    MultiplyCode ->
                        processMultiplyCode opt acc
                    StopCode ->
                        acc
        in
        processed) intList chunked

preProcessOptCodes : List Int -> List Int
preProcessOptCodes intList =
    let
        array = Array.fromList intList
        twelveReplace = Array.set 1 12 array
        twoReplace = Array.set 2 2 twelveReplace
    in
    Array.toList twoReplace

setNounAndVerbInMemory : Int -> Int -> List Int -> List Int
setNounAndVerbInMemory noun verb intList =
    let
        array = Array.fromList intList
        nounReplace = Array.set 1 noun array
        verbReplace = Array.set 2 verb nounReplace
    in
    Array.toList verbReplace

theURL : String
theURL =
    "https://jessewarden.com"

getRange : List Int
getRange =
    List.range 0 99

getRangeTuples : Int -> List (Int, Int)
getRangeTuples index =
    List.range 0 99
    |> List.map (\i -> (index, i))

getAllRangedTuples : List (List (Int, Int))
getAllRangedTuples =
    List.repeat 100 0
    |> List.indexedMap (\i index -> getRangeTuples i)

-- SUBSCRIPTIONS ---------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW ---------------------------------------------

view : Model -> Html Msg
view model =
    div []
        [
        -- exampleCard1 model
            div [ z4
                , style "margin" "12px"
                , style "width" "1024px"
                , style "height" "400px"
                , style "overflow" "auto"][
                    div [ style "display" "flex", style "flex-direction" "row"][
                        div [][
                            div [ headline5, style "padding" "8px" ][text "Advent of Code: Day 2"]
                            , div [ subtitle2, style "padding-left" "8px"][text "Calculate Opcodes"]
                        ]
                        , flexGrow2
                        , div [ style "padding" "12px"][ 
                            textButton { buttonConfig | onClick = Just LoadDefaultInput } "Load Input"
                            , textButton { buttonConfig | onClick = Just ParseInput } "Parse Input"
                            , textButton { buttonConfig | onClick = Just DetermineNounAndVerb } "Find Noun and Verb"
                        ]
                    ]
                    , div [ style "display" "flex", style "flex-direction" "row" ] [
                        div [ style "width" "120px" ] [
                            div [ ] [ b [] [text "Puzzle Input:"] ]
                            , textArea { textAreaConfig | value = model.initialInput, rows = Just 6, cols = Just 6}
                        ]
                        , viewIntRows model
                        , div [ style "width" "120px" ] [
                            div [ ] [ b [] [text "Processed:"] ]
                            , div [][text (processedOptCodesToString model.processedIntCodes)]
                        ]
                        , case model.targetNounVerbProgramResult of
                            Nothing ->
                                div [][]
                            Just target ->
                                div [ style "width" "120px" ] [
                                    div [ ] [ b [] [text "Noun:"] ]
                                    , div [][text (String.fromInt target.noun)]
                                    , div [ ] [ b [] [text "Verb:"] ]
                                    , div [][text (String.fromInt target.verb)]
                                    , div [ ] [ b [] [text "Challenge 2 Answer:"] ]
                                    , div [][text (String.fromInt model.challenge2Answer)]
                                ]
                    ]
                ]
        ]


viewIntRows : Model -> Html Msg
viewIntRows model =
    let
        chunked = chunk 4 model.intCodes
    in
    div
        [ style "width" "120px" ]
        ([div [ ] [ b [] [text "Rows:"] ]] ++ (List.map viewIntRow chunked))

processedOptCodesToString : List Int -> String
processedOptCodesToString list =
    let
        chunked = chunk 4 list
    in
    List.foldl (\optCode acc ->
        acc ++ (processedOptCodeToString optCode) ++ ",") "" chunked
    
processedOptCodeToString : List Int -> String
processedOptCodeToString list =
    List.foldl (\i acc -> acc ++ (String.fromInt i) ++ ", ") "" list 
    |> String.dropRight 2


viewIntRow : List Int -> Html Msg
viewIntRow list =
    let
        row =
            List.foldl (\i acc -> acc ++ (String.fromInt i) ++ ", ") "" list 
            |> String.dropRight 2
    in
    div [][ text row ]

flexGrow2 : Html Msg
flexGrow2 = 
    div [ style "flex-grow" "2" ][]

main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

puzzleInput : String
puzzleInput =
    """1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,23,5,27,2,27,10,31,1,31,9,35,1,35,5,39,1,6,39,43,2,9,43,47,1,5,47,51,2,6,51,55,1,5,55,59,2,10,59,63,1,63,6,67,2,67,6,71,2,10,71,75,1,6,75,79,2,79,9,83,1,83,5,87,1,87,9,91,1,91,9,95,1,10,95,99,1,99,13,103,2,6,103,107,1,107,5,111,1,6,111,115,1,9,115,119,1,119,9,123,2,123,10,127,1,6,127,131,2,131,13,135,1,13,135,139,1,9,139,143,1,9,143,147,1,147,13,151,1,151,9,155,1,155,13,159,1,6,159,163,1,13,163,167,1,2,167,171,1,171,13,0,99,2,0,14,0"""