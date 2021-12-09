import Data.Char      (digitToInt)
import Control.Monad
import Safe           (atMay)
import Data.Maybe     (catMaybes)

input :: IO [[Int]]
input = ((digitToInt <$>) <$>) . lines <$> readFile "input.txt"

main :: IO ()
main = do
       mappa <- input
       let points_value = low_points mappa
           res          = sum $ map (+1) points_value
       print res

low_points :: [[Int]] -> [Int]
low_points mappa = let coord = [(x, y) | x <- [0..(length (head mappa)-1)], y <- [0..(length mappa-1)]]
                   in map snd $ filter ((==0) . length . fst) $ find_sn mappa <$> coord

find_sn :: [[Int]] -> (Int, Int) -> ([Int], Int)
find_sn mappa (x, y) = let value = mappa !! y !! x
                       in ((filter (<=value) $ catMaybes $ up : down : right : left : []), value)
  where
       up    = extract x (y-1)
       down  = extract x (y+1)
       right = extract (x+1) y
       left  = extract (x-1) y
       extract a b = join $ (flip atMay a) <$> atMay mappa b

