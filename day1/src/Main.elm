module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, b)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

import Material.Button exposing (textButton, buttonConfig)
import Material.Card as Card exposing (CardActions, CardBlock, card, cardActionButton, cardActionIcon, cardActions, cardBlock, cardConfig, cardMedia, cardMediaConfig, cardPrimaryAction, cardPrimaryActionConfig)
import Material.Typography as Typography
import Material.Theme as Theme

import Debug exposing (log)
import String exposing (split)

type alias Model =
    { initialInput : String
    , moduleList : List Module
    , totalFuelRequired : Int
    , additionalFuelAccountingForFuelWeight : Int
    , finalFuelRequired : Int  }

type alias Module = {
    mass : Int
    , fuel : Int }

initialModel : Model
initialModel =
    { initialInput = ""
    , moduleList = []
    , totalFuelRequired = 0
    , additionalFuelAccountingForFuelWeight = 0 
    , finalFuelRequired = 0 }

init : Int -> ( Model, Cmd Msg )
init seed =
    ( initialModel, Cmd.none )

type Msg
    = LoadDefaultInput
    | ParseInput

-- SUBSCRIPTIONS ---------------------------------------------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadDefaultInput ->
           ( { model | initialInput = puzzleInput}, Cmd.none)
        ParseInput ->
            let
                moduleList =
                    parseInputs model.initialInput
                totalFuel =
                    calculateTotalFuelRequired moduleList
                
                -- additionalFuelAccountingForFuelWeight =
                --     calculateFuelForFuel totalFuel 0
                additionalFuelAccountingForFuelWeight =
                    List.map (\m -> calculateFuelForFuel m.fuel 0) moduleList
                    |> List.sum
                msg1 = log "additionalFuelAccountingForFuelWeight" additionalFuelAccountingForFuelWeight

                finalFuelRequired =
                    totalFuel + additionalFuelAccountingForFuelWeight

                -- Guess #1: 4970440, it's too high like the rent
                -- Guess #2 - Correct!: 4967616

            in
           ( { model 
                | moduleList = moduleList
                , totalFuelRequired = totalFuel
                , additionalFuelAccountingForFuelWeight = additionalFuelAccountingForFuelWeight
                , finalFuelRequired = finalFuelRequired }
                , Cmd.none )

parseInputs : String -> List Module
parseInputs inputString =
    let
        moduleList = 
            split "\n" inputString
            |> List.map String.toInt
            |> List.map (\m -> case m of
                                Just val -> val
                                Nothing -> 0)
            |> List.map (\int -> Module int (calculateMass int))
        -- msg1 = log "moduleList"  moduleList
    in
    moduleList

calculateMass : Int -> Int
calculateMass mass =
    let
        fuel =
            mass // 3 - 2
    in
    if fuel >= 0 then
        fuel
    else
        0

calculateTotalFuelRequired : List Module -> Int
calculateTotalFuelRequired moduleList =
    List.foldl (\m acc -> acc + m.fuel) 0 moduleList

-- let
--             additionalFuel =
--                 calculateMass mass
--             newFuelAmount = 
--                 additionalFuel + fuel
--             msg0 = log "0 ----" "------"
--             msg1 = log "1 mass:" mass
--             msg2 = log "2 fuel:" fuel
--             msg3 = log "3 additionalFuel:" mass
--             msg4 = log "4 newFuelAmount:" newFuelAmount
--         in

calculateFuelForFuel : Int -> Int -> Int
calculateFuelForFuel mass fuel =
    if mass > 0 then
        let
            additionalFuel =
                calculateMass mass
            newFuelAmount = 
                additionalFuel + fuel
        in
        if additionalFuel >= 0 then
            calculateFuelForFuel additionalFuel newFuelAmount
        else
            fuel
    else if mass == 0 then
        fuel
    else if mass < 0 then
        fuel
    else
        fuel


-- SUBSCRIPTIONS ---------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW ---------------------------------------------

view : Model -> Html Msg
view model =
    div []
        [
        exampleCard1 model
        ]


exampleCard1 : Model -> Html Msg
exampleCard1 model =
    card
        { cardConfig
            | additionalAttributes =
                [ Html.Attributes.style "margin" "48px 0"
                , Html.Attributes.style "width" "800px"
                ]
        }
        { blocks =
            cardPrimaryAction cardPrimaryActionConfig
                [demoTitle
                , demoBody model
                ]
        , actions = Just demoActions
        }


demoTitle : CardBlock msg
demoTitle =
    cardBlock <|
        Html.div
            [ Html.Attributes.style "padding" "1rem"
            ]
            [ Html.h2
                [ Typography.headline6
                , Html.Attributes.style "margin" "0"
                ]
                [ text "Our Changing Planet"
                ]
            , Html.h3
                [ Typography.subtitle2
                , Theme.textSecondaryOnBackground
                , Html.Attributes.style "margin" "0"
                ]
                [ text "by Kurt Wagner"
                ]
            ]


demoBody : Model -> CardBlock Msg
demoBody model =
    cardBlock <|
        Html.div
            [ style "padding" "0 1rem 0.5rem 1rem"
            , Typography.body2
            , Theme.textSecondaryOnBackground
            , style "display" "flex"
            , style "flex-direction" "row"
            ]
            [ 
                div [ style "width" "80px"] [
                    div [ ] [ b [] [text "Puzzle Input:"] ]
                    , div [] [ text model.initialInput ]
                ]
                , div [style "width" "200px", style "flex-grow" "2"] [
                    div [ ] [ b [] [text "Modules:"] ]
                    , div []
                        (List.map viewModule model.moduleList)
                        -- []
                ]
                , div [ style "width" "80px"] [
                    div [ ] [ b [] [text "Total Fuel:"] ]
                    , div [] [ text (String.fromInt model.totalFuelRequired) ]
                ]
                , div [ style "width" "80px"] [
                    div [ ] [ b [] [text "Additional Fuel For Fuel:"] ]
                    , div [] [ text (String.fromInt model.additionalFuelAccountingForFuelWeight) ]
                ]
                , div [ style "width" "80px"] [
                    div [ ] [ b [] [text "Final Fuel Required:"] ]
                    , div [] [ text (String.fromInt model.finalFuelRequired) ]
                ]
            ]

viewModule : Module -> Html Msg
viewModule moduleYo =
    div [style "width" "300px"][
        div [style "width" "120px"][
            b [] [text "Mass:"]
            , div [] [text (String.fromInt moduleYo.mass)]
        ]
        , div [style "width" "120px"] [
            b [] [text "Fuel:"]
            , div [] [text (String.fromInt moduleYo.fuel)]
        ]
    ]

demoActions : CardActions Msg
demoActions =
    cardActions
        { buttons =
            [ cardActionButton { buttonConfig | onClick = Just LoadDefaultInput } "Load Input"
            , cardActionButton { buttonConfig | onClick = Just ParseInput } "Parse Input"
            ]
        , icons =
            []
        }

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
    """75592
56081
141375
103651
132375
90584
94148
85029
95082
148499
108192
97739
60599
140308
125171
129160
143118
98762
103907
115389
127835
57917
72980
88747
86595
130407
116862
84652
112817
136922
51900
76677
146244
121897
99310
136486
84665
117344
88992
83929
74820
56651
74001
88636
51232
57878
114559
58879
145519
83727
111774
146256
123479
86955
64027
59812
59211
85835
58084
113676
119161
106368
137358
85290
81131
124857
51759
82977
138957
146216
147807
72265
60332
136741
110215
89293
148703
73152
93080
140220
68511
77397
51934
100243
92442
135254
98873
51105
118755
79155
89249
137430
142807
86334
117266
149484
89284
63361
52269
111666"""