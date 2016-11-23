module SilverMaple exposing (score)

{-| This module borrows heavily from [LiquidMetal](https://github.com/rmm5t/liquidmetal/)

-}

import Array
import Char
import String
import List.Extra as List
import Scores


score : String -> String -> Float
score text query =
    if (String.length query == 0) then
        Scores.trailing
    else if (String.length query > String.length text) then
        Scores.noMatch
    else
        let
            search =
                String.toLower text

            abbrev =
                String.toLower query
        in
            -- optimization: If scoreAll returns an empty array, return 0.0 (necessary?)
            scoreAll text search abbrev -1 0 [] []
                |> List.map List.sum
                |> List.maximum
                |> Maybe.withDefault 0.0


{-| ScoreAll takes:
  - string: The text you match against
  - search: String.downcase string
  - query: The string you are matching with
  - searchIndex: The index in search you are currently matching against
  - queryIndex: The index in query you are currently matching against
  - scores: the score list for this (what scope?)
  - allScores: accumulator for scores lists


-}
scoreAll : String -> String -> String -> Int -> Int -> List Float -> List (List Float) -> List (List Float)
scoreAll string search query searchIndex queryIndex scores allScores =
    -- Save completed match scores at end of search
    if (queryIndex == String.length query) then
        -- add trailing score for the remainder of the match
        -- save score clone since reference is persisted in scores
        --return
        allScores
    else
        -- perform matching
        let
            c =
                query
                    |> String.uncons
                    |> Maybe.withDefault ( '\x00', "" )
                    |> fst
                    |> String.fromChar

            instances =
                String.indexes c search

            myScores =
                instances
                    |> List.map (whileMoreCharacterInstances searchIndex search string scores)
                    |> List.transpose
                    |> List.map (List.maximum)
                    |> List.map (Maybe.withDefault 0.0)
        in
            -- Make sure there's at least one instance of the character available, or return
            -- match all instances of the abbreviation char
            -- consume matched string and recurse
            scoreAll string search query searchIndex (queryIndex + 1) myScores (myScores :: allScores)


whileMoreCharacterInstances :
    Int
    -> String
    -> String
    -> List Float
    -> Int
    -> List Float
whileMoreCharacterInstances scoreIndex search string scores index =
    -- TODO count the index in question as a match
    if (isNewWord string index) then
        -- TODO count the space as a match
        fillArray scores Scores.buffer (scoreIndex + 1) (index - 1)
    else if (isUpperCase string index) then
        fillArray scores Scores.buffer (scoreIndex + 1) index
    else
        fillArray scores Scores.noMatch (scoreIndex + 1) index


isNewWord : String -> Int -> Bool
isNewWord string index =
    let
        previousChar =
            String.toList string
                |> List.getAt (index - 1)
    in
        case previousChar of
            Just char ->
                List.member char wordSeparators

            Nothing ->
                True


wordSeparators : List Char
wordSeparators =
    String.toList " \t_-"


isUpperCase : String -> Int -> Bool
isUpperCase string index =
    string
        |> String.toList
        |> Array.fromList
        |> Array.get index
        |> Maybe.withDefault '\x00'
        |> Char.isUpper


fillArray : List Float -> Float -> Int -> Int -> List Float
fillArray scores value start end =
    List.concat
        [ List.take start scores
        , List.repeat (end - start) value
        , List.drop end scores
        ]


started : String -> String -> Bool
started string abbr =
    let
        s =
            List.head <| String.toList string

        a =
            List.head <| String.toList abbr
    in
        case ( s, a ) of
            ( Just s, Just a ) ->
                s == a

            _ ->
                False
