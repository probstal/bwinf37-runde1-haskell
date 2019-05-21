import Control.Monad (mapM_)
import Data.List ((\\), delete, sort)
import System.Environment (getArgs)

data ResistorSet
    = Series ResistorSet
             ResistorSet
    | Parallel ResistorSet
               ResistorSet
    | Resistor Double
    deriving (Show, Eq)

rEval :: ResistorSet -> Double
rEval (Series x y) = (rEval x) + (rEval y)
rEval (Parallel x y) = 1 / ((1 / rEval x) + (1 / rEval y))
rEval (Resistor o) = o

rDist :: ResistorSet -> ResistorSet -> Double
rDist x y = abs $ (rEval x) - (rEval y)

rFindSet :: Int -> Double -> [ResistorSet] -> ResistorSet
rFindSet _ _ [] = error "empty input"
rFindSet 1 goal set = foldr1 findClosest set
  where
    findClosest x y
        | rDist x (Resistor goal) < rDist y (Resistor goal) = x
        | otherwise = y
rFindSet k goal set = rFindSet 1 goal $ (combine set k)
  where
    combine set 1 = set
    combine set k = concat [typeCombine x (combine (x `delete` set) (k - 1)) | x <- set]
    typeCombine x set = concat [[Parallel x y, Series x y] | y <- set]

main :: IO ()
main = do
    args <- getArgs
    input <- getContents
    let resistors = map Resistor . map read . lines $ input
        stop = (length args) < 2
        goal = read . head $ args
        k =
            if (not $ stop)
                then sort . map read . tail $ args
                else [1 .. 4]
    putStrLn ""
    printMoves k goal resistors stop
  where
    printMoves (k:ks) goal resistors stop = do
        let solution = rFindSet k goal resistors
            value = rEval solution
            difference = rDist solution (Resistor goal)
        putStrLn $
            "k=" ++
            show k ++ " | " ++ show value ++ " (diff: " ++ show difference ++ "): " ++ show solution
        if (stop && not (null ks) && difference == 0.0)
            then mapM_ putStrLn $
                 [ ""
                 , "Info: Stopped checking for higher k's as difference can't be better than 0.0"
                 , "      Run program with explicit k parameter to force other solutions"
                 ]
            else printMoves ks goal resistors stop
    printMoves [] _ _ _ = return ()
