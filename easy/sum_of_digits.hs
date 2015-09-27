main :: IO ()
main = do
  putStrLn "Converting string of numbers to sum of digits."
  let str1 = "12345"
  let val = sumOfDigits str1
  print val
  putStrLn "Done"

charToInt :: Char -> Int
charToInt x = read [x]

sumOfDigits :: String -> Int
sumOfDigits x = sum ( map charToInt x)
