import Control.Monad (mapM_)
import Data.List ((\\))
import qualified Data.Map as Map (Map, fromListWith, lookup)

type Member = String
type Group = [Member]
type Blacklist = Group
type Log = [((Member, Member), Bool)]
type FollowMap = Map.Map Member Group

makeFollowMap :: [(Member, Member)] -> FollowMap
makeFollowMap = Map.fromListWith (++) . map (\(k, v) -> (k, [v]))

printLog :: Log -> IO ()
printLog log = do
    mapM_ putStrLn $ formatLog log

formatLog :: Log -> [String]
formatLog = map (\((m1, m2), v) -> m1 ++ " -> " ++ m2 ++ ": " ++ show v)

insertLog :: (Member, Member) -> Bool -> Log -> Log
insertLog k v = ((k, v) :)

lookupLog :: (Member, Member) -> Log -> Maybe Bool
lookupLog k =
    foldl
        (\acc (lk, lv) ->
             if k == lk
                 then Just lv
                 else acc)
        Nothing

follows :: Member -> Member -> FollowMap -> Bool
follows x y = contains . Map.lookup x
  where
    contains (Just a) = y `elem` a
    contains Nothing = False

followsLog :: Member -> Member -> Log -> Maybe Bool
followsLog x y = lookupLog (x, y)

possibleStar :: Group -> Log -> Blacklist -> Group -> FollowMap -> (Maybe Member, Log, Blacklist)
possibleStar [] log blacklist group followMap = (Nothing, log, blacklist)
possibleStar (reference:remaining) log blacklist group followMap
    | reference `elem` blacklist = possibleStar remaining log blacklist group followMap
    | otherwise = step reference (group \\ [reference]) log blacklist
  where
    step reference [] log blacklist = (Just reference, log, blacklist)
    step reference rest@(head:other) log blacklist =
        if (follows head reference followMap)
            then step reference other (insertLog (head, reference) True log) blacklist
            else if (not $ head `elem` blacklist)
                     then step
                              head
                              (group \\ [head])
                              (insertLog (head, reference) False log)
                              (reference : blacklist)
                     else possibleStar remaining log blacklist group followMap

confirmStar :: Maybe Member -> Log -> Group -> FollowMap -> (Maybe Member, Log)
confirmStar Nothing log _ _ = (Nothing, log)
confirmStar (Just m) log group followMap = step m (group \\ [m]) log
  where
    step m [] log = (Just m, log)
    step m (head:other) log =
        if (not $ doesFollow)
            then step m other newLog
            else (Nothing, newLog)
      where
        (doesFollow, newLog) = lookup (followsLog m head log) log
        lookup (Just a) log = (a, log)
        lookup Nothing log = (doesFollow', insertLog (m, head) doesFollow' log)
          where
            doesFollow' = follows m head followMap

determineStar :: Group -> FollowMap -> (Maybe Member, Log)
determineStar group followMap =
    (\(m, l, _) -> confirmStar m l group followMap) $ possibleStar group [] [] group followMap

parseInput :: String -> (Group, FollowMap)
parseInput = parse . lines
  where
    parse (x:xs) =
        (words x, makeFollowMap . map (\(k:v:_) -> (k, v)) . map words . filterEmpty $ xs)
    filterEmpty = filter ((> 0) . length)

main :: IO ()
main = do
    input <- getContents
    let (group, followMap) = parseInput input
        (m, log) = determineStar group followMap
        name (Just a) = a
        name Nothing = "<no superstar>"
    printLog $ reverse log
    putStrLn
        ("Superstar: " ++
         name m ++
         " (" ++ show (length log) ++ " requests for " ++ show (length group) ++ " group members)")
