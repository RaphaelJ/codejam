import Control.Applicative
import Data.Int
import Data.List
import Data.Function
import qualified Data.Foldable as F
import Text.Printf

import Debug.Trace

type StrLen = [(Char, Int)]
data Test = Test [StrLen]
    deriving Show

newtype Solution = Solution (Maybe Int)

instance Show Solution where
    show (Solution (Nothing)) = "Fegla Won"
    show (Solution (Just i) ) = show i

main = do
    interact (unlines . map showCase . zip [1..] . map (Solution . resoudre) . goTest . tail . lines)

  where
    goTest [] = []
    goTest (i':ls) =
        let i = read i'
            (strs, ls') = splitAt i ls
        in Test (map groupLen strs) : goTest ls'

    groupLen str = [ (head cs, length cs) | cs <- group str ]

    showCase :: (Int, Solution) -> String
    showCase (i, s) = printf "Case #%d: %s" i (show s)

resoudre :: Test -> Maybe Int
resoudre (Test strs) | all null strs = Just 0
                     | any null strs = Nothing
                     | otherwise     =
    let heads  = map head strs
        minLen = minimum $ map snd heads
        maxLen = maximum $ map snd heads
    in if not $ all (((fst (head heads)) ==) . fst) $ tail heads
       then Nothing
       else let changes = minimum [ sum [ abs (i - l) | (_, l) <- heads ]
                        | i <- [minLen..maxLen]
                        ]
            in (changes +) <$> resoudre (Test $ map tail strs)
