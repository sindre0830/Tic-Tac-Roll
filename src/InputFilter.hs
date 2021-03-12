module InputFilter
    ( 
    module InputFilter
    ) where
        
import Data.Char ( toLower )
import Text.Read ( readMaybe )
import Data.Maybe ( fromJust, isJust )

import Dictionary ( Input, Position, Direction, Size )

filterMenuInput :: Input -> (String, Size)
filterMenuInput input
    | input == "" = ("", 0)
    | otherwise = do
        let arrInp = words $ stringToLower input
        let cmd = head arrInp
        if length arrInp > 1
            then do
                let boardSize = readMaybe (arrInp !! 1) :: Maybe Size
                if isJust boardSize
                    then do
                        if fromJust boardSize > 1
                            then (cmd, fromJust boardSize)
                            else (cmd, 3)
                    else (cmd, 3)
            else (cmd, 3)


filterGameInput :: Input -> (Position, Direction)
filterGameInput inpStr
    | inpStr == "" = (0, "")
    | otherwise = do
        let arrInp = words $ stringToLower inpStr
        let pos = readMaybe (head arrInp) :: Maybe Position
        if isJust pos
            then if length arrInp > 1 && ((arrInp !! 1) == "left" || (arrInp !! 1) == "right")
                then (fromJust pos, arrInp !! 1)
                else (fromJust pos, "")
            else (0, "")

stringToLower :: String -> String
stringToLower = map toLower
