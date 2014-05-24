import Control.Applicative
import Data.Int
import Data.List
import Data.Function
import qualified Data.Foldable as F
import qualified Data.Set as S
import Text.Printf
import Data.Ratio

import Debug.Trace

type Train = String

data Test = Test [Train]
    deriving Show

newtype Solution = Solution Int

instance Show Solution where
    show (Solution i) = show i

main = do
    interact (unlines . map showCase . zip [1..] . map (Solution . resoudre) . goTest . tail . lines)

  where
    goTest [] = []
    goTest (_:l:ls) = Test (words l) : goTest ls

    showCase :: (Int, Solution) -> String
    showCase (i, s) = printf "Case #%d: %s" i (show s)

resoudre :: Test -> Int
resoudre (Test trains)
    | any (not . isValidTrain S.empty) trains' = 0
    | otherwise                                =
        sum [ go (S.fromList t) (last t) remains
            | (t, remains) <- selectTrain trains' []]
  where
    selectTrain []      ts2 = []
    selectTrain (t:ts1) ts2 = (t, ts2++ts1) : selectTrain ts1 (t:ts2)

    go letters lastLetter []      = 1
    go letters lastLetter remains =
        sum [ go (letters `S.union` S.fromList t) (last t) remains'
            | (t, remains') <- selectTrain remains []
            , head t == lastLetter || not (S.member (head t) letters)
            , all (\letter -> not (S.member letter letters)) (tail t)
            , not (any ) 
            ]

    trains' = map preprocessTrain trains

    preprocessTrain = map head . group

    isValidTrain _       []     = True
    isValidTrain letters (s:ss)
        | s `S.member` letters = False
        | otherwise            = isValidTrain (S.insert s letters) ss
