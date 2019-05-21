import Data.Char (isAlpha, toLower)
import Data.Function (on)
import Data.List ((\\), any, groupBy, intercalate, nub)
import qualified Data.Map as M
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Random

type Dictionary = [String]

tainit :: [a] -> [a]
tainit = tail . init

unmaybeList :: [a] -> Maybe [a] -> [a]
unmaybeList xs Nothing = xs
unmaybeList _ (Just xs) = xs

groupSentence :: String -> [String]
groupSentence = groupBy ((==) `on` isAlpha)

shuffle :: (RandomGen g, Eq a) => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen [x] = ([x], gen)
shuffle gen [x, y] = ([y, x], gen)
shuffle gen l =
    if (shuffled /= l)
        then r
        else shuffle gen' l
  where
    r@(shuffled, gen') = toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
    toElems (x, y) = (M.elems x, y)
    numerate = zip [1 ..]
    initial x gen = (M.singleton 0 x, gen)
    fisherYatesStep :: RandomGen g => (M.Map Int a, g) -> (Int, a) -> (M.Map Int a, g)
    fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m M.! j)) m, gen')
      where
        (j, gen') = randomR (0, i) gen

twistWord :: RandomGen g => g -> Dictionary -> String -> (String, g)
twistWord gen dic w
    | length w <= 3 = (w, gen)
    | any (not . isAlpha) w = (w, gen)
    | null dic = twist
    | not $ (map toLower w) `elem` dic = (w, gen)
    | otherwise = twist
  where
    twist = (head w : shuffled ++ [last w], gen')
      where
        (shuffled, gen') = shuffle gen $ tainit w

untwistWord :: Dictionary -> String -> [String]
untwistWord dic w
    | length w <= 3 = [w]
    | any (not . isAlpha) w = [w]
    | null dic = [w]
    | otherwise =
        map (\x -> head w : x ++ [last w]) .
        unmaybeList [tainit w] . M.lookup (numElems . map toLower . tainit $ w) $
        M.fromListWith (++) . map (\dw -> (numElems . tainit $ dw, [tainit dw])) $ fdic
  where
    fdic =
        filter (\dw -> head dw == (toLower . head) w && last dw == (toLower . last) w) .
        filter ((== length w) . length) $
        dic
    numElems :: (Num a, Ord k, Foldable t) => t k -> M.Map k a
    numElems = foldr (\c -> M.insertWith (+) c 1) M.empty

twistSentence :: RandomGen g => g -> Dictionary -> String -> (String, g)
twistSentence gen dic s = foldl twistStep ([], gen) $ groupSentence s
  where
    twistStep (s, gen) w = (\(tw, gen') -> (s ++ tw, gen')) $ twistWord gen dic w

untwistSentence :: Dictionary -> String -> String
untwistSentence dic s = foldl untwistStep [] $ groupSentence s
  where
    untwistStep s w = s ++ (formatWord . nub . untwistWord dic $ w)
    formatWord [] = ""
    formatWord [w] = w
    formatWord ws = "[" ++ (intercalate "/" ws) ++ "]"

shead :: [String] -> String
shead [] = ""
shead (x:_) = x

main :: IO ()
main = do
    args <- getArgs
    input <- getContents
    gen <- newStdGen
    let twist = not $ "-u" `elem` args
        wordPath = shead $ args \\ ["-u"]
        ls = lines input
    contents <-
        if (wordPath /= "")
            then readFile wordPath
            else return []
    let dic = map (map toLower) . lines $ contents
    if (twist)
        then putStr . unlines . map (\(s, _) -> s) . map (twistSentence gen dic) $ ls
        else putStr . unlines . map (untwistSentence dic) $ ls
