import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function
import Data.List
import Text.Printf

import Debug.Trace

main = do
    interact (unlines . map showCase . zip [1..] . go . tail . lines)

  where
    go :: [String] -> [Int]
    go []        = []
    go (l:l':ls) =
        let [size, n] = map read $ words l
            motes =  map read $ words l'
        in solve size (sort motes) n : go ls

    showCase :: (Int, Int) -> String
    showCase (i, r) = printf "Case #%d: %d" i r

solve :: Int -> [Int] -> Int -> Int
solve size [] n = 0
solve size ms'@(m:ms) n
    | size > m  = solve (size + m) ms (n-1)
    | size == 1 = n
    | otherwise = min n (1 + solve (size + size - 1) ms' n)