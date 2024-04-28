module Chapter9Exercises where

removeArticles :: String -> [String]
removeArticles = filter (\x -> notElem x articles) . words
  where articles = ["the", "an", "a"]
