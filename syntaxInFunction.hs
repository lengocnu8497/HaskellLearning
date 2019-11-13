lucky:: (Integral a) => a -> String
lucky 7 = "Lucky Number Seven!"
lucky x = "Sorry, ur out of luck"

sayMe 1 = "one"
sayMe 2 = "two"
sayMe x = "not one or two"
factorial 0 = 1
factorial n = n*factorial(n-1)

charName 'a' = "Albert"
charName 'b' = "Broseph"

addVectors (x1,y1) (x2,y2) = (x1 + x2, y1+y2)
head' :: [a] -> a
head' [] = error "empty list!"
head' (x:_) = x

length' [] = 0
length' (_:xs) = 1 + length' xs

sum' [] = 0
sum' (x:xs) = x + sum' xs 

capital all@(x:xs) = "first letter of " ++ all ++ " is " ++ [x]
bmiTell weight height | bmi <= 18.5 = "Unerweight" |bmi <= 25 = "normal" |bmi <= 30 = "overweight" where bmi = weight / height ^2

calcBmis xs = [bmi | (w,h) <- xs, let bmi = w/h^2, bmi >= 25]

describeList xs = "The list is " ++ case xs of [] -> "Empty string"  ;[x] -> "Singleton List"  ;xs -> "longer list"
