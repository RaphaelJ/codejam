import Data.Array
import qualified Data.Map as M
import Data.List
import Data.Maybe

import Debug.Trace

main = interact (showCase 1 . tail . lines)
  where
    showCase i [] = ""
    showCase i ls = 
        let (n, d, vs, ls') = parse ls
        in "Case #" ++ show i ++ ": " ++ boolToStr (solve n d vs) ++ "\n" ++ showCase (i + 1) ls'
   
parse :: [String] -> (Int, Int, [(Int, Int)], [String])
parse (l:ls) = 
    let n = read l :: Int
        (vs, (d:ls')) = splitAt n ls
        vs' = map (\[vd, vl] -> (read vd, read vl)) $ map words vs
    in (n, read d, vs', ls')
    
boolToStr True = "YES"
boolToStr False = "NO"

solve n d ((vd, vl):vs) = 
    fst $ solve' 0 vd M.empty vs
  where
    solve' pos dist acc vs'
        | pos + (dist * 2) >= d = (True, acc)
        | null vs'              = (False, acc)
        | otherwise             =
            let tests = takeWhile (\(vd', vl') -> vd' <= (dist * 2) + pos) vs'
            in loop pos dist tests acc
                      
    loop pos dist [] acc = (False, acc)
    loop pos dist (v:vs') acc =
        let pos' = traceShow (pos, dist, (pos + dist), (fst v - snd v)) $ max (pos + dist) (fst v - snd v)
            dist' = fst v - pos'
            (cond, acc') = solve' pos' (fst v - pos') acc (dropWhile (\v' -> fst v' <= fst v) vs')
            acc'' = M.insert (fst v) dist' acc'
        in case M.lookup (fst v) acc of
                Just dst | dst >= dist' -> (False, acc)
                         | otherwise  ->
                            if cond 
                                then (True, acc'')
                                else loop pos dist vs' acc''
                Nothing -> if cond 
                            then (True, acc'')
                            else loop pos dist vs' acc''
