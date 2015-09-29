import           Data.List (nub)

main :: IO ()
main = do
  putStrLn "Removing duplicate elements from list."
  let list1 = [1,1,1,2,2,3,3,4,4]
  print list1
  putStrLn "Removed duplicates"
  print (uniqueList list1)
  let list2 = [2,3,4,5,5]
  print list2
  putStrLn "Removed duplicates"
  print (uniqueList list2)
  putStrLn "Done"
  print list2
  print (nub list2)

uniqueList :: [Int] -> [Int]
uniqueList [] = []
uniqueList [x] = [x]
uniqueList (x:xs) = if x `elem` xs
                    then uniqueList xs
                    else x : uniqueList xs
