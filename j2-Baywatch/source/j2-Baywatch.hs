import Data.List

normalize :: [Char] -> [Char]
normalize xs = (/= ' ') `filter` xs

format :: [Char] -> [Char]
format xs = intersperse ' ' xs

rotate :: Int -> [Char] -> [Char]
rotate r xs = take (length xs) (drop r (cycle xs))

match :: [Char] -> [Char] -> Bool
match (x:xs) (y:ys) = (x == y || '?' `elem` [x, y]) && match xs ys
match [] [] = True

matches :: ([Char], [Char]) -> [Int] -> [Int]
matches x rs = [r | r <- rs, match template (rotate r list)]
  where
    (list, template) = x

toMaps :: [Char] -> [Int] -> [[Char]]
toMaps x = map (format . (flip rotate) x)

getMaps :: ([Char], [Char]) -> [[Char]]
getMaps x = list `toMaps` matches (list, template) [1 .. (length list)]
  where
    (list, template) = (normalize $ fst x, normalize $ snd x)

main :: IO ()
main = interact $ show . getMaps . (\(x:y:_) -> (x, y)) . lines . filter (/= '\r')
