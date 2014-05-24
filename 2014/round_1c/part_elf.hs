import Control.Applicative
import Data.Int
import Data.List
import Data.Function
import qualified Data.Foldable as F
import Text.Printf
import Data.Ratio

import Debug.Trace

data Test = Test Rational
    deriving Show

newtype Solution = Solution (Maybe Int)

instance Show Solution where
    show (Solution (Nothing)) = "impossible"
    show (Solution (Just i) ) = show i

main = do
    interact (unlines . map showCase . zip [1..] . map (Solution . resoudre) . goTest . tail . lines)

  where
    goTest [] = []
    goTest (l:ls) =
        let (num, _:denum) = span (/= '/') l
        in Test ((read num) % (read denum)) : goTest ls

    showCase :: (Int, Solution) -> String
    showCase (i, s) = printf "Case #%d: %s" i (show s)

resoudre :: Test -> Maybe Int
resoudre (Test ratio) | ratio == 0 = Nothing
                      | otherwise  = go ratio 0
  where
    go :: Rational -> Int -> Maybe Int
    go ratio gen | gen > 40   = Nothing
                 | otherwise  = {-traceShow (ratio, gen) $-}
        let frac = 1 % 2^gen
        in if frac <= ratio
              then let rest = ratio - frac * (truncate (ratio / frac) % 1)
                   in if rest == 0
                         then Just gen
                         else case go rest (gen + 1) of
                                   Just _  -> Just gen
                                   Nothing -> Nothing
              else go ratio (gen + 1)
