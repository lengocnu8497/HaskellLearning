doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y 
doubleSmallNumber x = if x > 100
					then x	
					else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) +1

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [ 1 | _ <- xs]
removeLowerCase st = [ c | c <- st, c `elem` ['A'..'Z']]

rightTriangles = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2, a+b+c == 24]
addThree :: Int -> Int -> Int -> Int
addThree x y z = x+y+z
factorial :: Integer -> Integer
factorial n = product [1..n]
fromIntegral :: (Num b, Integral) => a->b
