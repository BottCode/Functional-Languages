{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
    
import Log

{-Take the first word of a String-}
takeFirstWord :: String -> String

takeFirstWord [] = []

takeFirstWord (x:xs)
    | [x] /= " " = [x] ++ takeFirstWord xs
    | otherwise = []
 
dropFirstWord :: String -> String

dropFirstWord [] = []

dropFirstWord (x:xs)
    | [x] /= " " = dropFirstWord xs
    | otherwise xs


readWord :: String -> String

readWord w
    | w == "I" = "Information"
    | w == "E" = "Error " ++ readNumberError
    | w == "W" = "Warning"
    

{-parses an individual line from the log file.-}
parseMessage :: String -> LogMessage

parseMessage msg 
    


