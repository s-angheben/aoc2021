import Control.Arrow
import qualified Data.Map as M

main :: IO ()
main = do
    filedata <- readFile "input.txt"
    let (polymer, rules) = parse filedata
        result1           = iterate (flip step rules) polymer !! 10
        result2           = iterate (flip step rules) polymer !! 40
    print (calculate result1)
    print (calculate result2)  -- too slow :(

parse :: [Char] -> ([Char], M.Map [Char] Char)
parse = (head &&& (M.fromList . map (take 2 &&& last) . drop 2)) . lines

produce :: (a -> a) -> a -> [a]
produce f x = x : produce f (f x)

step :: [Char] -> M.Map [Char] Char -> [Char]
step template@(a:_) rules = let pair = take 2 template
                                xs   = drop 2 template
                            in
			       case M.lookup pair rules of
                                  Just x    -> ([head pair] ++ [x]) ++ step ((last pair):xs)  rules
			          otherwise -> pair ++ step xs rules
step _              _     = []

calculate :: [Char] -> Int
calculate xs = max - min
     where
       max = maximum $ (\l -> length $ filter (==l) xs) <$> ['A'..'Z']
       min = minimum $ filter (/=0)  $ (\l -> length $ filter (==l) xs) <$> ['A'..'Z']
