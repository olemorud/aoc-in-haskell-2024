
import Data.List -- sort
import Data.Map (Map)
import qualified Data.Map.Strict as Map.Strict
import qualified Data.Map as Map

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

countH :: Ord a => Map a Integer -> [a] -> Map a Integer
countH m (x:xs) = countH newmap xs
    where newmap = Map.Strict.insertWith (+) x 1 m
countH m _ = m

count :: Ord a => [a] -> Map a Integer
count = countH Map.empty

applyToSecond :: (b->c) -> (a, b) -> (a, c) 
applyToSecond f (x, y) = (x, f y)

toColumnsH :: [a] -> [a] -> [a] -> ([a], [a])
toColumnsH ls rs (x:xs) = toColumnsH rs (ls++[x]) xs
toColumnsH ls rs _ = (ls, rs)

toColumns :: [a] -> ([a], [a])
toColumns = toColumnsH [] []

similarityH :: Integer -> Map Integer Integer -> [Integer] -> Integer
similarityH acc m (x:xs) = similarityH (acc + x * (Map.findWithDefault 0 x m)) m xs
similarityH acc m _ = acc

similarity :: ([Integer], Map Integer Integer) -> Integer
similarity (xs, m) = similarityH 0 m xs

main = interact $ show . similarity . applyToSecond count . mapTuple sort . toColumns . map read . words
