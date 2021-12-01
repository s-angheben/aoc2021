module Main where

import Control.Monad.State

check :: Int -> State Int Bool
check new = state (\old -> (old<new, new))

countInc :: [Int] -> State Int Int
countInc = foldM incIf 0 where
  incIf :: Int -> Int -> State Int Int
  incIf n i = do
    res <- check i
    if res then return (n+1) else return n

sumWin :: [Int] -> Int -> [Int] -> [Int]
sumWin xs w acc = 
  if length xs < w 
    then 
      acc
    else 
      let f = ((foldr (+) 0) .  (take w))
          newacc = acc ++ [f xs]
          newxs  = drop 1 xs
      in 
        sumWin newxs w newacc

main = do
  input_data <- readFile "input.txt"
  let nums = read <$> lines input_data :: [Int]
      res = evalState (countInc nums) (head nums)
  putStr ("first sol: ")
  print res

  let second_nums = sumWin nums 3 []
      second_res = evalState (countInc second_nums) (head second_nums)
  putStr ("second sol: ")
  print second_res



