
isSortedGradually' :: (Integer -> Integer -> Bool) -> Bool -> [Integer] -> Bool
isSortedGradually' f mercy (x1:x2:xs) =
    (diff >= 1 && diff <= 3 && f x1 x2 && isSortedGradually' f mercy (x2:xs))
        || mercy && isSortedGradually' f False (x1:xs)
       where diff = abs (x1 - x2)
isSortedGradually' f mercy _ = True

isSortedGradually f (x:xs) = isSortedGradually' f True (x:xs)
                          || isSortedGradually' f False xs

isSafe :: [Integer] -> Bool
isSafe xs = isSortedGradually (<) xs || isSortedGradually (>) xs

main :: IO() 
main = interact $ show . sum . map (fromEnum . isSafe . map read . words) . lines
