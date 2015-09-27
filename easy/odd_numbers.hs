main :: IO ()
main = do
  putStrLn "Printing odd numbers from 1 - 100"
  putStrLn "Using list comprehensions"
  print oddNumbers
  putStrLn "Done."
  putStrLn "Using functions"
  print oddNumbers'
  putStrLn "Done."

oddNumbers :: [Int]
oddNumbers = [ x | x <- [1..100], odd x ]

oddNumbers' :: [Int]
oddNumbers' = takeWhile (<100) (filter odd [1..])
