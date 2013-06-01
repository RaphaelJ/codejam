import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function
import Data.List
import Text.Printf

import Debug.Trace

price n d = let i = n - d
            in ((n^2 + n - 2) `div` 2) - (((i * (i + 1)) `div` 2) - 1)

main = do
    interact (unlines . map showCase . zip [1..] . go . tail . lines)

  where
    go :: [String] -> [Int]
    go []     = []
    go (l:ls) =
        let [n, nmaps] = map read $ words l
            (people, ls') =  splitAt nmaps ls
        in solve n 0 [] (sortBy (compare `on` fst) $ map readMap people) : go ls'

    readMap l = let [start, stop, n] = map read l in (start, (stop, n))

    showCase :: (Int, Int) -> String
    showCase (i, r) = printf "Case #%d: %d" i (r `mod` 1000002013)

--       Saved       (Start,   (Stop,    n))       -> 
solve :: Integer -> Integer -> [(Integer, (Integer, Integer)]
                            -> [(Integer, (Integer, Integer)] -> Integer
solve stops saved _  []     = saved
solve stops saved [] (o:os) = solve saved o os
solve stops saved is'@(i@(start, (stop, n)):is) os'@(o@(ostart, (ostop, on')):os)
    | stop < ostart = solve saved is os' -- Personnes descendues
    | n == 0   = solve stops is os'
    | on' == 0 = solve stops is' os
    | stop < ostop = -- Swap avec un dans le train qui va partir avant.
        if n > on'
            then (start, (oStop, nSwap)) : (start, (stop, n - nSwap)) : (ostart, (stop, nSwap)) : is
            else (start, 
   | otherwise =
       
    reverse $ sortBy (compare `on` dist)
   =

    dist (sa, (so, _)) = so - sa

    price' = price stops

    nSwap = min n on'

    p  = price' (dist i) + price' (dist o)
    p' =  price' (dist i') + price' (dist o')

    saved = nSwap * (p - p')

    i' = (ostart, (stop, nSwap))
    iRemain = (ostart, (stop, n - nSwap))
    o' = (start, (ostop, nSwap))
    oRemain = (ostart, (stop, n - nSwap))
    
    sorted = sortBy (compare `on` start) . sortBy (compare `on` stop)