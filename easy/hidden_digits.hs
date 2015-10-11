--  for codeeval challenge #122 https://www.codeeval.com/open_challenges/122/
--  Hidden Digits challenge
--  Input example:  abcdefghik
--                  Xa,}A#5N}{xOBwYBHIlH,#W
--  a-j corresponds to the numbers 0-9, and the numbers correspond to themselves.
--  any other character means nothing, and the problem is to print out only meaningful characters.
--  Should output:  012345678
--                  05

import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.Environment
import           System.IO


main = do
  cmdArgs <- getArgs
  let filePath = head cmdArgs
  output <- withFile filePath ReadMode (\handle -> do
    contents <- hGetContents handle
    strings <- forM (lines contents) (\line -> return $ (convertEmptyOutput . findHiddenDigits) line)
    mapM putStrLn strings)
  return output

convertEmptyOutput :: String -> String
convertEmptyOutput "" = "NONE"
convertEmptyOutput x = x

findHiddenDigits :: String -> String
findHiddenDigits "" = ""
findHiddenDigits str = mapCharacter (head str) ++ findHiddenDigits (tail str)

mapCharacter :: Char -> String
mapCharacter c =  if getNum (elemIndex c numbers) < 10
                  then c : ""
                  else mapCharacterFromLetter c
                  where numbers = ['0'..'9']

mapCharacterFromLetter :: Char -> String
mapCharacterFromLetter c =  if getNum index < 10
                            then (show . getNum) index
                            else ""
                            where letters = ['a'..'j']
                                  index = elemIndex c letters

getNum :: Maybe Int -> Int
getNum = fromMaybe (10)
