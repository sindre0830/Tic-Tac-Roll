module InputFilter
    ( 
    module InputFilter
    ) where
-- foreign modules
import Data.Char ( toLower )
import Text.Read ( readMaybe )
import Data.Maybe ( fromJust, isJust )
-- local modules
import Dictionary ( Input, Position, Direction, Size, Command )
-- | Filters menu input into command and valid boardSize.
filterMenuInput :: Input -> (Command, Size)
filterMenuInput input
    | input == "" = ("", 0)
    | otherwise = do
        let arrInp = words $ stringToLower input
        let cmd = head arrInp
        if length arrInp > 1
            then do
                let boardSize = readMaybe (arrInp !! 1) :: Maybe Size
                -- branch if input is a valid size
                if isJust boardSize && fromJust boardSize > 1
                    then (cmd, fromJust boardSize)
                    else (cmd, 3)
            else (cmd, 3)
-- | Filters game input into valid position and direction.
filterGameInput :: Input -> (Position, Direction)
filterGameInput inpStr
    | inpStr == "" = (0, "")
    | otherwise = do
        let arrInp = words $ stringToLower inpStr
        let pos = readMaybe (head arrInp) :: Maybe Position
        -- branch if input is a valid position
        if isJust pos
            then if length arrInp > 1 && ((arrInp !! 1) == "left" || (arrInp !! 1) == "right")
                then (fromJust pos, arrInp !! 1)
                else (fromJust pos, "")
            else (0, "")
-- | Converts string to lowercase.
stringToLower :: String -> String
stringToLower = map toLower
