import Data.List
import Text.Printf

import qualified Data.Vector as V

import Debug.Trace

data Quaternions = One
                 | Minus Quaternions
                 | I
                 | J
                 | K
    deriving (Eq, Show)

data Test = Test {
      string    :: [Quaternions]
    , nRepeat   :: Int
    } deriving Show

newtype Solution = Solution { possible :: Bool }

instance Show Solution where
    show (Solution t) = show t

main = do
    interact (unlines . map showCase . zip [1..] . map resolve . goTest . tail . lines)

  where
    goTest [] = []
    goTest (l1:l2:ls) =
        let [_, nReplicate] = map read $ words l1
            str             = map toQuaternions l2
        in Test str nReplicate : goTest ls

    toQuaternions 'i' = I
    toQuaternions 'j' = J
    toQuaternions 'k' = K

    showCase :: (Int, Solution) -> String
    showCase (i, Solution True)  = printf "Case #%d: YES" i
    showCase (i, Solution False) = printf "Case #%d: NO" i

mult One        a           = a
mult a          One         = a
mult I          I           = Minus One
mult I          J           = K
mult I          K           = Minus J
mult J          I           = Minus K
mult J          J           = Minus One
mult J          K           = I
mult K          I           = J
mult K          J           = Minus I
mult K          K           = Minus One
mult (Minus a)  (Minus b)   = mult a b
mult a          (Minus b)   = case mult a b of Minus c -> c
                                               c       -> Minus c
mult (Minus a)  b           = case mult a b of Minus c -> c
                                               c       -> Minus c

resolve :: Test -> Solution
resolve (Test _   0) = Solution False
resolve (Test []  _) = Solution False
resolve (Test str n) =
    Solution $! go (V.head strReplicated) (V.tail strReplicated) [I, J, K]
  where
    strReplicated = V.fromList $ concat $ replicate n str

    go acc xs [y]    | V.null xs = acc == y
    go acc xs (y:ys) | acc == y && not (V.null xs) && go (V.head xs) (V.tail xs) ys =
        True
    go acc xs ys     | not (V.null xs) = go (mult acc (V.head xs)) (V.tail xs) ys
    go _   _      _      = False

{-
resolve :: Test -> Solution
resolve (Test _   0) = Solution False
resolve (Test []  _) = Solution False
resolve (Test str n) =
    Solution $! go strReplicated [I, J, K]
  where
    strReplicated   = concat $ replicate n str

    go []        []                          = True
    go (x:xs)    (y:ys) | x == y && go xs ys = True
    go (x:x':xs) ys                          = go ((mult x x'):xs) ys
    go _         _                           = False-}
