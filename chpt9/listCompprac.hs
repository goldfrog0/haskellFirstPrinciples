acro :: String -> String
acro phrase =
    [x | x <- phrase, elem x ['A'..'Z']]

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

tupSQ = [(x,y) | x <- mySqr, y <- myCube]

tupSQlt50= [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]


itIsMystery xs =
    map (\x -> elem x "aeiou") xs

--create a funciton that removes the articles "the", "a", and "an" from sentences

remArticles :: String -> [String]
remArticles sentence = clean
    where
    tt = (/= "the")
    a = (/= "a")
    n = (/= "an")
    clean = filter n  (filter a ( filter tt  (words sentence)))



