--  for codeeval challenge #71 https://www.codeeval.com/open_challenges/71/
--  ONE ZERO, TWO ZERO challenge
--  Input example:  1,2,3,4,5;2
--                  1,2,3,4,5;3
--  Reverse the data by groups of n. If the list is not a multiple of n, leave the last elements as they are
--  Should output:  2,1,4,3,5
--                  3,2,1,4,5

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           System.Environment
import           System.IO

main = do
  cmdArgs <- getArgs
  let filePath = head cmdArgs
  output <- withFile filePath ReadMode (\handle -> do
    contents <- hGetContents handle
    counts <- forM (lines contents) (\line -> return $ splitAndApply $ getData line)
    mapM (putStrLn . show) counts)
  return output

splitAndApply :: (String, Int) -> String
splitAndApply (str, n) = concat ( intersperse "," ( reverseGroups n ( splitOn "," str ) ) )

getData :: String -> (String, Int)
getData str = let tokens = splitAt ( fromMaybe 0 $ elemIndex ';' str) str
              in (fst tokens, read $ tail $ snd tokens :: Int)

reverseGroups :: Int -> [String] -> [String]
reverseGroups n xs =  concat $ (map reverse $ filter ((==n).length) $ chunksOf n xs)
                      ++
                      (filter ((/=n).length) $ chunksOf n xs)
