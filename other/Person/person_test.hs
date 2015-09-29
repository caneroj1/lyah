import           Person

main :: IO ()
main = do
  putStrLn "Making a person"
  let p1 = Person{firstName = "Bob", lastName = "Smith", city = "Town", state = "NJ", address = "20 Happy Way", Person.zip = "11234", age = 50}
  let p2 = Person{firstName = "Harry", lastName = "Albert", city = "Town", state = "PA", address = "50 Burger Place", Person.zip = "11234", age = 22}
  let peopleList = [p1, p2]
  print "Printing Sorted"
  print $ sortPeople peopleList
  print "Printing people with first name = 'Bob'"
  print $ peopleWith (firstName) "Bob" peopleList
