
import Data.List -- sort

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

-- helper function for split
splitH :: [a] -> [a] -> [a] -> ([a], [a])
splitH ls rs (x:xs) = splitH rs (ls++[x]) xs
splitH ls rs _ = (ls, rs)

-- split: split list into two by alternating between left and right
split :: [a] -> ([a], [a])
split = splitH [] []

-- calculates the sum of the absolute distance between two lists
dist :: Int -> ([Int], [Int]) -> Int
dist acc (l:ls, r:rs) = dist (acc + abs (r - l)) (ls, rs)
dist acc _ = acc

main = interact $ show . dist 0 . mapTuple sort . split . map (read :: String -> Int) . words
