import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function
import Data.List
import Text.Printf

import Debug.Trace

main = do
    interact (unlines . map showCase . zip [1..] . map go . tail . lines)

  where
    go :: String -> Rational
    go l = let [n, x, y] = map read $ words l
           in solve S.empty 0 n (x, y)

    showCase :: (Int, Rational) -> String
    showCase (i, r) = printf "Case #%d: %f" i (fromRational r :: Double)

solve :: S.Set (Int, Int) -> Int -> Int -> (Int, Int) -> Rational
solve st initY 0 dest = 0
solve st initY n dest =
    fall st 0 initY
  where
    fall st x y
        | y == 0 || (left && right) =
            let st' = S.insert (x, y) st
            in if (x, y) == dest then 1
                                 else solve st' 100 (n-1) dest
        | left  = fall st (x+1) (y-1)
        | right = fall st (x-1) (y-1)
        | S.member (x, y-2) st =
            (fall st (x-1) (y-1) + fall st (x+1) (y-1)) / 2
        | otherwise = fall st x (y-2) -- y - 2 ?
      where
        left  = S.member (x-1, y-1) st
        right = S.member (x+1, y-1) st

--     go :: String -> Rational
--     go l = let [n, x, y] = map read $ words l
--            in solve M.empty 0 0 n (x, y)
-- 
--     showCase :: (Int, Rational) -> String
--     showCase (i, r) = printf "Case #%d: %f" i (fromRational r :: Double)
-- 
-- solve :: M.Map (Int, Int) Rational -> Int -> Int -> (Int, Int) -> Rational
-- solve st _ _ 0 (x, y) = getVal st x y    
-- solve st y i n _      =
--     fall 0 y
--   where
--     fall x 0 = 
--         M.insertWith (+ 
--     fall x y = 
--         
--     
--     
-- getVal st x y | Just v <- (x, y) `M.lookup` st = v
--               | otherwise                      = 0