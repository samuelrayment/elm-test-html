module HtmlRunner (runDisplay) where

import ElmTest.Run as Run
import ElmTest.Test exposing (..)
import ElmTest.Runner.String as String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


numericStatusMessage : (List Attribute -> List Html -> Html) -> String -> String -> Int -> Html
numericStatusMessage element class' message count =
    element [ class class' ]
            [ text (message ++ ": ")
            , span [] [text (toString count)]
            ] 


createDivsForResults : Run.Result -> Html
createDivsForResults results =
    let
        createDiv' result =
            case result of
                Run.Pass name ->
                    div [ style [("color", "green")]
                        , class "test"
                        ]
                        [ text ("Pass: " ++ name) ]
                Run.Fail name errorMessage ->
                    div [ style [("color", "red")]
                        , class "test"
                        ]
                        [ text ("Fail: " ++ name)
                        , span [] [ text errorMessage ]
                        ]
                Run.Report name result' ->
                    div [ class "report" ]
                        [ text name
                        , div [ class "tests" ] <| List.map createDiv' result'.results
                        ]
    in
        div [] <| [createDiv' results]


statusMessage : Run.Result -> List Html
statusMessage results =
    let
        passedTests'  = Run.passedTests results
        passedSuites' = Run.passedSuites results
        failedTests'  = Run.failedTests results
        failedSuites' = Run.failedSuites results
        passed        = failedTests' == 0
        passedText    = if passed then "Passed" else "Failed"    
    in
    [ h1 [class "status"] [ text ("Tests: " ++ passedText) ]
    , numericStatusMessage h2 "suitepass" "Test Suites Passed" passedSuites'
    , numericStatusMessage h2 "suitefail" "Test Suites Failed" failedSuites'
    , numericStatusMessage h2 "passcount" "Tests Passed" passedTests'
    , numericStatusMessage h2 "failcount" "Tests Passed" failedTests'
    ]


runDisplay : Test -> Html
runDisplay tests =
    let
        results = Run.run tests
        failedTests'  = Run.failedTests results
        passed        = failedTests' == 0
        passClass     = if passed then "passed" else "failed"
    in
    div [ class ("testresults " ++ passClass) ]
        <| ((statusMessage results) ++ [createDivsForResults results])