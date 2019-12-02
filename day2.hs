import qualified Data.Map as Map
import Data.List.Split

type State = Map.Map Int Int

-- Part 1
makeState :: [Int] -> State
makeState = Map.fromList . (zip [0..])

runInstruction :: Int -> State -> (State, Maybe Int)
runInstruction i s = case instruction of
    1  -> (Map.insert target (e1 + e2) s, Just $ i + 4)
    2  -> (Map.insert target (e1 * e2) s, Just $ i + 4)
    99 -> (s, Nothing)
  where
    instruction = s Map.! i
    pos1 = s Map.! (i+1)
    pos2 = s Map.! (i+2)

    e1 = s Map.! pos1
    e2 = s Map.! pos2

    target = s Map.! (i+3)

runInstructions :: State -> State
runInstructions = go 0
  where
    go :: Int -> State -> State
    go i s = case next of
        Just n  -> go n s'
        Nothing -> s'
      where
        (s', next) = runInstruction i s

runProgram :: Int -> Int -> [Int] -> [Int]
runProgram noun verb = Map.elems . runInstructions . (restoreState noun verb) . makeState

restoreState :: Int -> Int -> State -> State
restoreState n v s = Map.insert 1 n $ Map.insert 2 v $ s

findOutput :: Int -> [Int] -> (Int, Int)
findOutput target ins = fst $ head $ filter (\x -> snd x == target) $ pairs
  where
    pairs = (\(n,v) -> ((n,v), head $ runProgram n v ins)) <$> inputs
    inputs = [(n,v) | n <- [0..99], v <- [0..99]]

findPart2 :: Int -> [Int] -> Int
findPart2 t p = 100 * n + v
  where
    (n, v) = findOutput t p

main :: IO ()
main = do
    contents <- getContents
    let program = read <$> splitOn "," contents
    putStrLn " -- Part 1 --"
    putStrLn $ show $ runProgram 12 2 program
    putStrLn " -- Part 2 --"
    putStrLn $ show $ findPart2 19690720 program
