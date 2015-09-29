import           Data.List

main :: IO ()
main = do
  let list1 = [1..5]
  let list2 = [5,4..1]
  print $ intercalate [-1] [list1, list2]
  print $ intersperse 'a' "hello"
  print $ replicate 3 $ splitAt 2 list2
  print $ take 15 $ cycle [1..3]
  print $ let myList = take 15 $ cycle [1..3] in map length $ group $ sort myList
  print "Done"
