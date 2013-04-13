import Data.Array
import Data.List
import Data.Maybe

main = interact (showCase 1 . tail . lines)
  where
    showCase i [] = ""
    showCase i ls = 
        let (h, a, ls') = parse ls
        in "Case #" ++ show i ++ ": " ++ show (solve h a) ++ "\n" ++ showCase (i + 1) ls'
   
parse :: [String] -> (Int, Array (Int, Int) (Int, Int), [String])
parse (l:ls) = 
    let [h, n, m] = map read $ words $ l
        (ns, ls') = splitAt (2*n) ls
        (ceilings, floors) = splitAt n ns
        ceilings' = concat $ map (map read . words) ceilings
        floors' = concat $ map (map read . words) floors
        a = listArray ((0, 0), (n-1, m-1)) $ zip ceilings' floors'
    in (h, a, ls')
    

solve h a = show a
    