{- 
    Jesse Warden
    jesterxl@jessewarden.com
    jesse.warden@gmail.com
    Twitter: twitter.com/jesterxl
    YouTube: youtube.com/jessewarden
    https://adventofcode.com/2019/day/1
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

-- import Debug exposing (log)
import String exposing (split)

type alias Model =
    { initialInput : String
    , moduleList : List Module
    , totalFuelRequired : Int
    , additionalFuelAccountingForFuelWeight : Int
    , finalFuelRequired : Int
    , largestMass : Int  }

type alias Module = {
    mass : Int
    , fuel : Int
    , fuelForFuel : Int }

initialModel : Model
initialModel =
    { initialInput = ""
    , moduleList = []
    , totalFuelRequired = 0
    , additionalFuelAccountingForFuelWeight = 0 
    , finalFuelRequired = 0
    , largestMass = 0 }

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
                
                additionalFuelAccountingForFuelWeight =
                    List.map (\m -> calculateFuelForFuel m.fuel 0) moduleList
                    |> List.sum
                -- msg1 = log "additionalFuelAccountingForFuelWeight" additionalFuelAccountingForFuelWeight

                finalFuelRequired =
                    totalFuel + additionalFuelAccountingForFuelWeight

                largestMass =
                    calculateHighestMass moduleList
                -- Guess #1: 4970440, it's too high like the rent
                -- Guess #2 - Correct!: 4967616

            in
           ( { model 
                | moduleList = moduleList
                , totalFuelRequired = totalFuel
                , additionalFuelAccountingForFuelWeight = additionalFuelAccountingForFuelWeight
                , finalFuelRequired = finalFuelRequired
                , largestMass = largestMass }
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
            |> List.map (\int -> Module int (calculateMass int) (calculateFuelForFuel (calculateMass int) 0))
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
    
calculateHighestMass : List Module -> Int
calculateHighestMass moduleList =
    List.foldl (\m acc -> if m.mass > acc then
                            m.mass
                          else
                            acc) 0 moduleList

-- additionalFuelAccountingForFuelWeight =
--     List.map (\m -> calculateFuelForFuel m.fuel 0) moduleList
--     |> List.sum
    
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
        -- exampleCard1 model
            div [ z4
                , style "margin" "12px"
                , style "width" "1024px"
                , style "height" "400px"
                , style "overflow" "auto"][
                    div [ style "display" "flex", style "flex-direction" "row"][
                        div [][
                            div [ headline5, style "padding" "8px" ][text "Advent of Code: Day 1"]
                            , div [ subtitle2, style "padding-left" "8px"][text "Calculate Fuel Requirements"]
                        ]
                        , flexGrow2
                        , div [ style "padding" "12px"][ 
                            textButton { buttonConfig | onClick = Just LoadDefaultInput } "Load Input"
                            , textButton { buttonConfig | onClick = Just ParseInput } "Parse Input"
                        ]

                    ]
                    , div
                        [ style "padding" "1rem 1rem 1rem 1rem"
                        , Typography.body2
                        , Theme.textSecondaryOnBackground
                        , style "display" "flex"
                        , style "flex-direction" "row"
                        ]
                        [ 
                            div [ style "width" "120px" ] [
                                div [ ] [ b [] [text "Puzzle Input:"] ]
                                , textArea { textAreaConfig | value = model.initialInput, rows = Just 6, cols = Just 6}
                            ]
                            , div [style "flex-grow" "2"] [
                                div [][
                                    b [] [text "Modules:"]
                                    , table 
                                        [style "width" "100%"]
                                        ((
                                            [tr [caption][
                                                th [ style "text-align" "right" ][text "Mass"]
                                                , th [style "text-align" "right"][text "Fuel"]
                                                , th [style "text-align" "right"][text "Fule For Fuel"]
                                                , th [style "text-align" "left"][]
                                            ]
                                            ])
                                            ++ (List.map (viewModule model.largestMass) model.moduleList)
                                        )
                                        
                                ]
                            ]
                            , div [ style "width" "140px"] [
                                div [ ] [ b [] [text "Total Fuel:"] ]
                                , div [] [ text (formatNumber model.totalFuelRequired) ]
                                , div [ ] [ b [] [text "Fuel For Fuel:"] ]
                                , div [] [ text (formatNumber model.additionalFuelAccountingForFuelWeight) ]
                                , div [ ] [ b [] [text "Final Fuel Required:"] ]
                                , div [] [ text (formatNumber model.finalFuelRequired) ]
                            ]
                            , flexGrow2
                        ]
                ]
        ]

flexGrow2 : Html Msg
flexGrow2 = 
    div [ style "flex-grow" "2" ][]

viewModule : Int -> Module -> Html Msg
viewModule largestMass moduleYo =
    tr [][
        td [style "text-align" "right"][text (formatNumber moduleYo.mass)]
        , td [style "text-align" "right"][text (formatNumber moduleYo.fuel)]
        , td [style "text-align" "right"][text (formatNumber moduleYo.fuelForFuel)]
        , td [style "text-align" "left"][viewProgressBar largestMass moduleYo]
    ]

calculateRoundPercentage largestMassFloat value =
    round( (value / largestMassFloat) * 100 )

viewProgressBar : Int -> Module -> Html Msg
viewProgressBar largestMass moduleYo =
    let
        largestMassFloat = toFloat largestMass
        calc = calculateRoundPercentage largestMassFloat
        massFloat = toFloat moduleYo.mass
        fuelFloat = toFloat moduleYo.fuel
        fuelFromFuel = toFloat moduleYo.fuelForFuel

        theEighty = calc massFloat
        massWidth = (String.fromInt theEighty) ++ "px"
        fuelWidth = (String.fromInt (calc fuelFloat) ) ++ "px"
        fuelForFuelWidth = (String.fromInt (calc fuelFromFuel) ) ++ "px"
        -- msg1 = log "largestMassFloat" largestMassFloat
        -- msg2 = log "massFloat" massFloat
        -- msg3 = log "total" (massFloat / largestMassFloat)
    in
    
    div [style "width" "150px"
        , style "height" "12px"
        , style "display" "flex"
        , style "flex-direction" "row"
        , style "padding-left" "8px"
        , style "-webkit-mask-image" "url(mask.png)"
        , style "padding-right" "1px"][
        div [ style "background-color" "#90EE02"
            , style "width" massWidth
            , style "height" "12px"][]
        , div [ style "background-color" "#41C300"
            , style "width" fuelWidth
            , style "height" "12px"][]
        , div [ style "background-color" "#008B00"
            , style "width" fuelForFuelWidth
            , style "height" "12px"][]
    ]


fuelLocale : Locale
fuelLocale =
    { usLocale
        | decimals = 0
    }

formatNumber : Int -> String
formatNumber val =
    format fuelLocale (toFloat val)

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