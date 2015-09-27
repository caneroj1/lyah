main :: IO ()
main = do
  putStrLn "Finding position of rightmost character in a string...."

  let str1 = "Hello, World!"
  let str2 = "Joseph Canero"

  putStr ("Position of rightmost 'l' in '" ++ str1 ++ "': ")
  print (rightmostPosition str1 'l')
  putStr ("\nPosition of rightmost 'e' in '" ++ str2 ++ "': ")
  print (rightmostPosition str2 'e')

rightmostPosition :: String -> Char -> Int
rightmostPosition [] _ = -1
rightmostPosition [l] c = if l == c then 0 else -1
rightmostPosition s c = if last s == c then length s - 1 else rightmostPosition (init s) c
