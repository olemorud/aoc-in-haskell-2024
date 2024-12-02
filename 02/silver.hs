
isSortedGradually :: (Integer -> Integer -> Bool) -> [Integer] -> Bool
isSortedGradually f (x1:x2:xs) = (f x1 x2) && diff >= 1 && diff <= 3 && isSortedGradually f (x2:xs)
    where diff = abs (x1 - x2)
isSortedGradually f _ = True

isSafe :: [Integer] -> Bool
isSafe xs = isSortedGradually (<) xs || isSortedGradually (>) xs

main :: IO() 
main = interact $ show . sum . map (fromEnum . isSafe . map (read :: (String -> Integer)) . words) . lines
