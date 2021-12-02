import Text.Parsec

--            x   y
data Movs = F Int Int
          | D Int Int
          | U Int Int
        deriving (Show, Read)


fname = "input.txt"

calculate :: (Int, Int) -> Movs -> (Int, Int)
calculate (x, y) (F m_x m_y) = (m_x+x, m_y+y)  
calculate (x, y) (D m_x m_y) = (m_x+x, m_y+y)  
calculate (x, y) (U m_x m_y) = (m_x+x, m_y+y)  

main :: IO()
main = do
  text <- readFile fname
  let movs_str = lines text
      Right (movs) = sequence $ map (\a -> parse parse_all "" a) movs_str
      coordinate = foldl calculate (0,0) movs
      res = uncurry (*) coordinate

  putStr "coordinate: "
  print coordinate
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
	  f "forward" n = F n 0
	  f "down"    n = D 0 n
	  f "up"      n = U 0 (-n)
	  f s _         = error $ "invalid" ++ s

