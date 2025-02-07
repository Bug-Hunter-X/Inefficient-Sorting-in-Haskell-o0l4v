```haskell
import Data.List (sort)
import Data.Function (on)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
  then x : merge xs (y:ys)
  else y : merge (x:xs) ys

main :: IO ()
main = do
  let xs = [5, 2, 8, 1, 9, 4, 7, 3, 6]
  let ys = mergeSort xs
  print ys

-- Using mergeSort significantly improves the time complexity to O(nlogn)
```