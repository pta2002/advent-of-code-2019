-- Part 1
massForModule :: Double -> Int
massForModule n = (floor (n / 3)) - 2

massTotal :: [Double] -> Int
massTotal = sum . (map massForModule)

-- Part 2
massForFuel :: Int -> Int
massForFuel x
  | massForModule (fromIntegral x) <= 0 = x + 0
  | otherwise = x + massForFuel (fromIntegral $ massForModule (fromIntegral x))

massFinal :: Int -> Int
massFinal = massForFuel . massForModule . fromIntegral

massTotal2 :: [Int] -> Int
massTotal2 = sum . (map massFinal)

main :: IO ()
main = do
    contents <- getContents
    let masses = read <$> lines contents :: [Int]
    putStrLn $ show $ massTotal (fromIntegral <$> masses)
    putStrLn $ show $ massTotal2 masses

