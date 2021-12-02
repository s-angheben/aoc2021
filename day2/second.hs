import Text.Parsec

data Movs = F Int
          | D Int
          | U Int
        deriving (Show, Read)


fname = "input.txt"

--            x    y    aim
calculate :: (Int, Int, Int) -> Movs -> (Int, Int, Int)
calculate (x, y, aim) (F value) = 
	let
	  calc = value*aim
	in
	  (x+value, y+calc, aim)
calculate (x, y, aim) (D value) = (x, y, aim+value)
calculate (x, y, aim) (U value) = (x, y, aim-value)

moltiply :: (Int, Int, Int) -> Int
moltiply (a, b, c) = a*b

main :: IO()
main = do
  text <- readFile fname
  let movs_str = lines text
      Right (movs) = sequence $ map (\a -> parse parse_all "" a) movs_str
      res = moltiply $ foldl calculate (0,0,0) movs
  putStr "response: "
  print res

parse_word :: Parsec String () [Char]
parse_word  = many1 letter

parse_num :: Parsec String () Int
parse_num   = read <$> many1 digit

parse_all :: Parsec String () Movs
parse_all = do
	s <- parse_word
	char ' '
	n <- parse_num
	return (f s n)
	where
	  f "forward" n = F n
	  f "down"    n = D n
	  f "up"      n = U n
	  f s _         = error $ "invalid" ++ s

