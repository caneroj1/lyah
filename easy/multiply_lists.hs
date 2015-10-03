--  for codeeval easy challenge #113 https://www.codeeval.com/open_challenges/113/
--  Multiply Lists challenge
--  Input example: 9 0 6 | 15 14 9
--  Multiply the corresponding elements in each list
--  Should output: 135 0 54

import           Control.Monad
import           Data.List.Split
import           System.Environment
import           System.IO

main = do
  cmdArgs <- getArgs
  let path = head cmdArgs
  output <- withFile path ReadMode (\handle -> do
    contents <- hGetContents handle
    results <- forM (lines contents) (\line -> return $ processLine line)
    mapM putStrLn results)
  return output

processLine :: String -> String
processLine = prettifyOutput . multiplyLists . mapToNums . splitAtPipe

prettifyOutput :: [Int] -> String
prettifyOutput [] = ""
prettifyOutput (x:xs) = show x ++ " " ++ prettifyOutput xs

multiplyLists :: [[Int]] -> [Int]
multiplyLists xs = zipWith (*) (head xs) (last xs)

splitAtPipe :: String -> [String]
splitAtPipe = splitOn " | "

mapToNums :: [String] -> [[Int]]
mapToNums xs = [ map toInt x | x <- map words xs]

toInt :: String -> Int
toInt x = read x :: Int
