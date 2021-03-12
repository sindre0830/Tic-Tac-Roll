module InputFilter
    ( 
    module InputFilter
    ) where
        
import Data.Char ( toLower )
import Text.Read ( readMaybe )
import Data.Maybe ( fromJust, isJust )

import Dictionary ( Input, Position, Direction )

stringToLower :: String -> String
stringToLower = map toLower

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
