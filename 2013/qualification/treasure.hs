import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function
import Data.List
import Text.Printf

import Debug.Trace

data Chest = Chest { cChestIndex :: Int, cKeys :: [Int] } deriving (Eq, Ord)

main = do
    interact (unlines . map showCase . zip [1..] . go . tail . lines)

  where
    go :: [String] -> [Maybe [Int]]
    go []        = []
    go (l:l':ls) =
        let [nKeys, nChests] = map read $ words l
            keys = M.fromListWith (+) $ [ (read k, 1) | k <- words l' ]
            (lsChest, ls') = splitAt nChests ls
            chests = M.fromListWith (S.union)
                                    [ (typ, S.singleton (Chest i ks))
                | (i, lChest) <- zip [1..] lsChest
                , let (typ:_:ks) = map read $ words lChest
                ]
        in solve keys chests : go ls'

    showCase :: (Int, Maybe [Int]) -> String
    showCase (i, Nothing) = printf "Case #%d: IMPOSSIBLE" i
    showCase (i, Just ts) =
        printf "Case #%d: %s" i (unwords $ map show $ ts)

solve :: M.Map Int Int -> M.Map Int (S.Set Chest) -> Maybe [Int]
solve keys chests | M.null chests = Just []
                  | M.null keys   = Nothing
                  | otherwise     = do
    let opennable = filter ((`M.member` keys) . fst) $ M.toList chests

    msum [ (ix :) `fmap` solve keys'' chests'
        | (k, cs) <- opennable
        , c@(Chest ix ks) <- sortBy (compare `on` cChestIndex) $ S.toList cs
        , let cs' = S.delete c cs
        , let chests' = if S.null cs'
                then M.delete k chests
                else M.insert k cs' chests
        , let keys' = M.update (\n -> if n == 1 then Nothing else Just $ n - 1)
                               k keys
        , let keys'' = foldl' (\m k-> M.insertWith (+) k 1 m) keys' ks
        ]
