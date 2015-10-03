--  for codeeval easy challenge #97 https://www.codeeval.com/open_challenges/97/
--  Find a Writer challenge
--  Input example: osSE5Gu0Vi8WRq93UvkYZCjaOKeNJfTyH6tzDQbxFm4M1ndXIPh27wBA rLclpg| 3 35 27 62 51 27 46 57 26 10 46 63 57 45 15 43 53
--  Use the values on the right of the pipe to pick letters from the left.
--  Should output: Stephen King 1947

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
    authors <- forM (lines contents) (\line -> return $ findAuthor (mapData (splitAt (findPipe line) line)))
    mapM putStrLn authors)
  return output

findPipe :: String -> Int
findPipe = (+ 1) . fromMaybe 0 . elemIndex '|'

mapData :: (String, String) -> (String, [Int])
mapData (x,y) = (x, map (\s -> (read s :: Int) - 1) (words y))

findAuthor :: (String, [Int]) -> String
findAuthor (s, l) = map (s !!) l
