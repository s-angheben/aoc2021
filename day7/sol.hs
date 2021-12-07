input :: IO [Int]
input = read . ('[':) . (++"]") <$> readFile "input.txt"

main :: IO ()
main = do
        positions <- input
        let l     = maximum positions
            res1  = minimum $ map (cost1 positions) [0..l]
	    med   = div (sum positions) (length positions)
	    res2  = min (cost2 positions (med)) (cost2 positions (med +1))
        putStr "min cost1: " 
        print res1
        putStr "min cost2: " 
        print $ res2

cost1 :: [Int] -> Int -> Int
cost1 xs val = let a = zip xs (take (length xs) [val,val..])
               in sum $ map (\(x,y) -> abs $ x-y) a

cost2 :: [Int] -> Int -> Int
cost2 xs val = let a = zip xs (take (length xs) [val,val..])
               in sum $ map (\(x,y) -> sum [1..(abs (x-y))]) a
