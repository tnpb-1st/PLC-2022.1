factorial :: Integer -> Integer
factorial 1 = 1
factorial x = x * factorial(x-1)

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"