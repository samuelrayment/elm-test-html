module ElmTestHtmlRunner (runDisplay) where
{- Test runner for Elm-Test using elm-html for output. -}
    
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


indentation : List (String, String)
indentation = [("padding-left", "10px")]


passStyle : List (String, String)
passStyle = [("color", "green")]


failStyle : List (String, String)
failStyle = [("color", "red")]


decideStyle : Bool -> List (String, String)
decideStyle pass = if pass then passStyle else failStyle
    

createDivsForResults : Run.Result -> Html
createDivsForResults results =
    let
        createDiv' result =
            case result of
                Run.Pass name ->
                    div [ style (passStyle ++ indentation)
                        , class "test pass"
                        ]
                        [ text ("Pass: " ++ name) ]
                Run.Fail name errorMessage ->
                    div [ style (failStyle ++ indentation)
                        , class "test fail"
                        ]
                        [ text ("Fail: " ++ name)
                        , div [class "expection"
                              , style indentation
                              ] [ text errorMessage ]
                        ]
                Run.Report name result' ->
                    div [ class "report"
                        , style (indentation ++ (decideStyle <| List.length result'.failures == 0))
                        ]
                        (text name :: List.map createDiv' result'.results)
                        
    in
        div [class "results"] <| [createDiv' results]


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
    div [ class ("testresults " ++ passClass)
        , style indentation
        ] <| ((statusMessage results) ++ [createDivsForResults results])