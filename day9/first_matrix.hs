import Data.Char      (digitToInt)
import Control.Monad
import Data.Maybe     (catMaybes)
import Data.Matrix as M

input :: IO (Matrix Int)
input = M.fromLists . ((digitToInt <$>) <$>) . lines <$> readFile "input.txt"

main :: IO ()
main = do
       mappa <- input
       let points_value = low_points mappa
           res          = sum $ map (+1) points_value
       putStr "first solution: "
       print res

low_points :: Matrix Int -> [Int]
low_points mappa = let coord = [(x, y) | x <- [1..(M.ncols mappa)], y <- [1..(M.nrows mappa)]]
                   in map snd $ filter ((==0) . length . fst) $ find_sn mappa <$> coord

find_sn :: Matrix Int -> (Int, Int) -> ([Int], Int)
find_sn mappa (y,x) = let value = M.getElem x y mappa
                      in ((filter (<=value) $ catMaybes $ up : down : right : left : []), value)
  where
       up    = M.safeGet x (y-1) mappa
       down  = M.safeGet x (y+1) mappa
       right = M.safeGet (x+1) y mappa
       left  = M.safeGet (x-1) y mappa


