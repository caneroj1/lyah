--  for codeeval challenge #217 https://www.codeeval.com/browse/217/
--  ONE ZERO, TWO ZERO challenge
--  Input example:  1 8
--                  2 4
--  The first number is the target, and the second is the range.
--  Print out the amount of numbers that have the target number of zeros in
--  1 to range.
--  Should output:  3
--                  1

import           Control.Monad
import           Data.List
import           System.Environment
import           System.IO

main = do
  cmdArgs <- getArgs
  let filePath = head cmdArgs
  output <- withFile filePath ReadMode (\handle -> do
    contents <- hGetContents handle
    counts <- forM (lines contents) (\line -> return $ numbersThatSatisfy line)
    mapM (putStrLn . show) counts)
  return output

convertToBinaryString :: Int -> String
convertToBinaryString 0 = "0"
convertToBinaryString 1 = "1"
convertToBinaryString x = convertToBinaryString ( quot x 2 ) ++ (if odd x then "1" else "0")

numberOfZeros :: String -> Int
numberOfZeros x = length $ elemIndices '0' x

matchNumber :: Int -> Int -> Bool
matchNumber target = (target==) . numberOfZeros . convertToBinaryString

convertStringToNumber :: String -> Int
convertStringToNumber x = read x :: Int

numbersThatSatisfy :: String -> Int
numbersThatSatisfy line = length $ elemIndices True $ map (matchNumber target) [1..range]
                          where items = processInput line
                                target = head items
                                range = last items

processInput :: String -> [Int]
processInput x = map convertStringToNumber $ words x
