module InputFilter
	( 
	module InputFilter
	) where
        
import Data.Char ( toLower )

import Dictionary ( Input, Position, Direction )

stringToLower :: String -> String
stringToLower = map toLower

filterGameInput :: Input -> (Position, Direction)
filterGameInput inpStr = do
	let arrInp = words $ stringToLower inpStr
	let pos = read (head arrInp) :: Position
	if length arrInp > 1 && ((arrInp !! 1) == "left" || (arrInp !! 1) == "right")
		then (pos, arrInp !! 1)
		else (pos, "")
