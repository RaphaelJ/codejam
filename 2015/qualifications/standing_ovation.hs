import Data.Char (digitToInt)
import Data.List
import qualified Data.Foldable as F
import Text.Printf

import Debug.Trace

data Test = Test {
      shyness  :: [Int]
    } deriving Show

newtype Solution = Solution Int

instance Show Solution where
    show (Solution t) = show t

main = do
    interact (unlines . map showCase . zip [1..] . map resolve . goTest . tail . lines)

  where
    goTest [] = []
    goTest (l:ls) =
        let [_, str] = words l
        in Test (map digitToInt str) : goTest ls

    showCase :: (Int, Solution) -> String
    showCase (i, s) = printf "Case #%d: %s" i (show s)

resolve :: Test -> Solution
resolve t =
    Solution $ go (shyness t) 0 0
  where
    go [] _ _ = 0
    go (0:xs)    shyLevel nLowerLevels = go xs (shyLevel + 1) nLowerLevels
    go (nShy:xs) shyLevel nLowerLevels =
        let toInvite      = max 0 (shyLevel - nLowerLevels)
        in toInvite + go xs (shyLevel + 1) (nLowerLevels + toInvite + nShy)
