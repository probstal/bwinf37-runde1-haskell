ladders :: [(Int, Int)]
ladders =
    [ (6, 27)
    , (14, 19)
    , (21, 53)
    , (31, 42)
    , (33, 38)
    , (46, 62)
    , (51, 59)
    , (57, 96)
    , (65, 85)
    , (68, 80)
    , (70, 76)
    , (92, 98)
    ]

maxPosition = 100 :: Int

getLadderEnd :: Int -> [(Int, Int)] -> Int
getLadderEnd n (x:xs)
    | fst x == n = snd x
    | snd x == n = fst x
    | otherwise = getLadderEnd n xs
getLadderEnd _ _ = -1

calculatePosition :: Int -> Int -> Int
calculatePosition position stepSize
    | ladderEnd /= -1 = ladderEnd
    | otherwise = newPosition
  where
    newPosition =
        if (sum > maxPosition)
            then maxPosition - (sum - maxPosition)
            else sum
      where
        sum = position + stepSize
    ladderEnd = getLadderEnd newPosition ladders

getMovesRemaining :: Int -> [Int] -> Int
getMovesRemaining stepSize trace
    | position /= maxPosition && not (newPosition `elem` trace) =
        getMovesRemaining stepSize (trace ++ [newPosition])
    | position == maxPosition = (length trace) - 1
    | otherwise = -1
  where
    position = last trace
    newPosition = calculatePosition position stepSize

main = printMoves [1 .. 6]
  where
    printMoves (x:xs) = do
        putStrLn $ "Moves with " ++ show x ++ " steps: " ++ show (getMovesRemaining x [0])
        printMoves xs
    printMoves [] = return ()
