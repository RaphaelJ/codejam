import Control.Applicative
import Data.Int
import Data.List
import Data.Bits
import Data.Function
import qualified Data.Foldable as F
import Text.Printf

import Debug.Trace

data Test = Test Int Int Int
    deriving Show

newtype Solution = Solution Int

instance Show Solution where
    show (Solution i) = show i

main = do
    interact (unlines . map showCase . zip [1..] . map resoudre . goTest . tail . lines)

  where
    goTest [] = []
    goTest (l:ls) =
        let [a, b, k] = map read $ words l
        in Test a b k : goTest ls

    showCase :: (Int, Solution) -> String
    showCase (i, s) = printf "Case #%d: %s" i (show s)

resoudre :: Test -> Solution
resoudre (Test a b k) =
    Solution $ go a b k 30
  where
    go a b k bit
        | bit < 0        = 1 -- Fin
        | bitSet >= maxi = go a b k bit' -- Ne peut être généré
        | bitSet < mini  = -- Peut-être annulé
            if bitSet > k
               then -- Ne peut-être à 1 dans k, doit être annulé.
                    go a' b' k bit' + go a b k bit'
               else -- Peut être à 1 dans k, teste les combinaisons
                      go a' b k' bit' + go a  b' k' bit' -- Actif
                    + go a  b k  bit' + go a' b' k  bit' -- Inactif
        | bitSet >= mini = -- Doit être à 1 dans le k car ne peut-être annulé
            if bitSet > k
               then -- Ne peut-être à 1 dans le k et ne peut donc être actif
                    go a b k bit'
               else -- Peut-être à 1 dans le k et peut donc être actif
                      go (maxi - bitSet) mini k' bit'
                    + go a b k bit'

      where
        (!mini, !maxi) = (min a b, max a b)

        bitSet = 1 `shiftL` bit

        bit' = bit - 1

        a' = a - bitSet
        b' = b - bitSet
        k' = k - bitSet

--     Solution $ length [ () | a' <- [0..a-1], b' <- [0..b-1]
--                            , a' .&. b' < k ]
