import Data.Int
import Data.List.Split
import Data.Sequence hiding (zip, length, reverse, filter)
import qualified Data.Foldable as F
import Text.Printf

import Debug.Trace

main = do
    interact (unlines . map showCase . zip [1..] . map (solve . interval) . tail . lines)

  where
    interval :: String -> (Integer, Integer)
    interval l =
        let [a, b] = map read $ splitOn " " l
        in (a, b)

    showCase :: (Int, Int) -> String
    showCase (i, r) = printf "Case #%d: %d" i r

solve :: (Integer, Integer) -> Int
solve (a, b) = length $ traceShow (takeWhile (<= b) $ dropWhile (< a) numbers)
                                   (takeWhile (<= b) $ dropWhile (< a) numbers)

numbers :: [Integer]
numbers =
    filter palindrome $ map (\x -> x*x) $ filter palindrome $ map number $ go 1
  where
    go :: Int -> [Seq Int]
    go len
        | len <= 0 = go 1
        | len == 1 = map singleton [1..9] ++ go (len + 1)
        | otherwise = [ (i <| inner) |> i
            | inner <- go0 (len - 2), i <- [1..9]
            ] ++ go (len + 1)

    go0 :: Int -> [Seq Int]
    go0 len
        | len <= 0 = [empty]
        | len == 1 = map singleton [0..9]
        | otherwise = [ (i <| inner) |> i
            | inner <- go0 (len - 2), i <- [0..9]
            ]

palindrome s =
    let s' = show s
    in s' == reverse s'

number :: Seq Int -> Integer
number =
    fst . F.foldr step (0, 0)
  where
    step x (acc, i) = 
        let x' = fromIntegral x
        in (acc + x' * 10^i, i+1)
