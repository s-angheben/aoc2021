import Data.List
import Data.Bits

data Bit = Zero | One
  deriving (Show, Eq, Ord)

-- False -> One
-- True  -> Zero
most_freq :: Ord c => Bool -> [c] -> c
most_freq p c = snd . my_max . map (\xs -> (length xs, head xs)) . group . sort $ c
      where
        my_max :: (Foldable t, Ord a, Ord b) => t (a, b) -> (a, b)
        my_max = foldr1 (\a@(i1,b1) b@(i2, b2) -> if i1 == i2 
	                                           then if p then a else b 
	                                           else max a b)
less_freq :: Ord c => Bool -> [c] -> c
less_freq p c = snd . my_min . map (\xs -> (length xs, head xs)) . group . sort $ c
      where
        my_min :: (Foldable t, Ord a, Ord b) => t (a, b) -> (a, b)
        my_min = foldr1 (\a@(i1,b1) b@(i2, b2) -> if i1 == i2 
	                                           then if p then a else b 
	                                           else min a b)

to_dec :: [Bit] -> Int
to_dec h = sum $ g (f h) 
  where
    f h = zip h (reverse ([0..(length h)-1])) 
    g   = ((\(b, pos) -> case b of
    			   One  -> (2^pos)
			   Zero -> 0
			   ) <$>)

filter_ox :: [Bit] -> [[Bit]] -> [Bit]
filter_ox v d = f v d 0
	where
          f :: [Bit] -> [[Bit]] -> Int -> [Bit]
          f v d n = let current = filter (\a -> take n a == take n v) d
	                new_v   = (most_freq False) <$> (transpose current)
                    in if length current == 1 then head current 
		                               else f new_v current (n+1)
filter_co :: [Bit] -> [[Bit]] -> [Bit]
filter_co v d = f v d 0
	where
          f :: [Bit] -> [[Bit]] -> Int -> [Bit]
          f v d n = let current = filter (\a -> take n a == take n v) d
	                new_v   = (less_freq True) <$> (transpose current)
                    in if length current == 1 then head current 
		                               else f new_v current (n+1)

neg :: Bit -> Bit
neg Zero = One
neg One  = Zero

extract :: [Char] -> [Bit]
extract []        = []
extract ('0': xs) = Zero : (extract xs)
extract ('1': xs) = One  : (extract xs)
extract (_: xs)   = undefined

main :: IO ()
main = do
   txt <- readFile "input.txt"
   let bit_data     = map extract (words txt)
       gamma_rate   = (most_freq True) <$> (transpose bit_data)
       epsilon_rate = map neg gamma_rate
       gamma_rate_int   = to_dec gamma_rate
       epsilon_rate_int = to_dec epsilon_rate
   putStr "Gamma rate: " 
   print (gamma_rate)
   putStr "epsilon rate: "
   print (epsilon_rate)

   putStr "response: "
   print (gamma_rate_int * epsilon_rate_int)

   let oxigen = filter_ox gamma_rate bit_data
       co2    = filter_co epsilon_rate bit_data
       res    = product . map to_dec $ [oxigen, co2]
   print oxigen
   print co2
   print res 


