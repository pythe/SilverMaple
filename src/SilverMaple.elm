module SilverMaple exposing (score)

{-| This module borrows heavily from [LiquidMetal](https://github.com/rmm5t/liquidmetal/)

-}

import String
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
            List.sum <| scoreAll text search abbrev -1 0 [] []


{-| ScoreAll takes:
  - string: The text you match against
  - search: String.downcase string
  - query: The string you are matching with
  - searchIndex: The index in search you are currently matching against
  - queryIndex: The index in query you are currently matching against
  - scores: the score list for this (what scope?)
  - allScores: accumulator for scores lists


-}
scoreAll : String -> String -> String -> Int -> Int -> List Float -> List Float -> List Float
scoreAll string search query searchIndex queryIndex scores allScores =
    -- Save completed match scores at end of search
    if (queryIndex == String.length query) then
        -- add trailing score for the remainder of the match
        -- save score clone since reference is persisted in scores
        --return
        []
    else
        -- perform matching
        let
            c =
                case List.head <| String.toList query of
                    Nothing ->
                        '\x00'

                    Just char ->
                        char
        in
            -- cancel match if a character is missing
            -- match all instances of the abbreviation char
            -- consume matched string and recurse
            scoreAll string search query searchIndex (queryIndex + 1) scores allScores


started : String -> String -> Bool
started string abbr =
    let
        s =
            List.head <| String.toList string

        a =
            List.head <| String.toList abbr
    in
        s == a


wordSeparators : String
wordSeparators =
    " \t_-"
