-- pythagorean triples if x^2+y^2=z^2
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2+y^2==z^2]

-- perfect integer
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPerfect :: Int -> Bool
isPerfect x = sum (init (factors x)) == x

perfects :: Int -> [Int]
perfects n = [p | p <- [1..n], isPerfect p]

--scalar product
scalarprod :: [Int] -> [Int] -> Int
scalarprod xs ys = sum [(x * y) | (x, y) <- zip xs ys ]

sp :: [Int] -> [Int] -> Int
sp xs ys = sum [xs !! i * ys !! i | i <- [0..n-1]]
  where n = length xs