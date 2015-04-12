import Data.Array
import Data.List
import Data.List
import Data.List
import Text.Printf

import qualified Data.Foldable  as F
import qualified Data.Map       as M

import Debug.Trace

data Test = Test {
      pancakes :: [Int]
    } deriving Show

newtype Solution = Solution { minutes :: Int }

instance Show Solution where
    show (Solution t) = show t

main = do
    interact (unlines . map showCase . zip [1..] . map resolve . goTest . tail . lines)

  where
    goTest [] = []
    goTest (_:l:ls) =
        let xs = map read $ words l
        in Test xs : goTest ls

    showCase :: (Int, Solution) -> String
    showCase (i, s) = printf "Case #%d: %s" i (show s)

resolve :: Test -> Solution
resolve (Test []) = Solution 0
resolve (Test ps) =
    let groups = groupN $ sortBy (flip compare) ps
    in Solution $ go groups
  where
    go []                          = 0
    go ((nGroup, nPancakes) : ps') =
        let div2 = nPancakes `div` 2
            mod2 = div2 + nPancakes `mod` 2
--         in if nGroup < nPancakes
        in if nPancakes > 1
              then 
                   let rest | even nPancakes = insertGroup (nGroup * 2, div2) ps'
                            | odd  nPancakes = insertGroup (nGroup, mod2) $
                                               insertGroup (nGroup, div2) ps'
                   in min (nGroup + go rest) nPancakes
              else nPancakes

-- resolve :: Test -> Solution
-- resolve (Test []) = Solution 0
-- resolve (Test ps) =
--     Solution $ go ps
--   where
--     go []  = 0
--     go ps' =
--         let wait = 1 + go (filter (> 0) $ map pred ps')
--             move | null (filter (> 1) ps') = wait
--                  | otherwise               =
--                     minimum $ select ps' (\x xs ->
--                         if x > 1
--                             then Just $ 1 + go ((x `div` 2) : (x `div` 2 + x `mod` 2) : xs)
--                             else Nothing)
--         in min wait move
-- 
-- select :: [Int] -> (Int -> [Int] -> Maybe a) -> [a]
-- select xs f =
--     go 0 xs
--   where
--     go _ []     = []
--     go i (y:ys) =
--         case f y (take i xs ++ ys) of
--             Just res -> res : go (i + 1) ys
--             Nothing  -> go (i + 1) ys

insertGroup :: Ord a => (Int, a) -> [(Int, a)] -> [(Int, a)]
insertGroup (nGroup, nPancakes) xs =
    let (greaters, lessers) = span ((> nPancakes) . snd) xs
    in if not (null lessers) && snd (head lessers) == nPancakes
            then greaters ++ (nGroup + fst (head lessers), nPancakes)
                            : tail lessers
            else greaters ++ (nGroup, nPancakes) : lessers

groupN :: Eq a => [a] -> [(Int, a)]
groupN = map (\(x:xs) -> (length xs + 1, x)) . group
