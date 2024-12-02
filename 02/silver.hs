
import Debug.Trace

debug = flip trace

isSortedGradually :: (Integer -> Integer -> Bool) -> [Integer] -> Bool
isSortedGradually f (x1:x2:xs) = (f x1 x2) && diff >= 1 && diff <= 3 && isSortedGradually f (x2:xs)
    where diff = abs (x1 - x2)
isSortedGradually f (x1:_) = True
isSortedGradually f _ = True

isAscending  :: [Integer] -> Bool
isAscending = isSortedGradually (<)
isDescending :: [Integer] -> Bool
isDescending = isSortedGradually (>)

ok xs = isAscending xs || isDescending xs

main :: IO() 
main = interact $ show . sum . map (fromEnum . ok . map (read :: (String -> Integer)) . words) . lines
