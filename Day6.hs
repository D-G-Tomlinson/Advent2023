import System.IO
import Data.List.Split

part1 :: IO ()
part1 = do
  input <- readFile "textFiles/day6"
  print (doP1 input)
    where
      doP1 :: String -> Int
      doP1 input = product (zipWith getNumWays times distances)
        where
          [l1, l2] = lines input
          times     :: [Float]
          (_:times)     = map read ((filter (not.null)) (splitOn " " l1))
          distances :: [Float]
          (_:distances) = map read ((filter (not.null)) (splitOn " " l2))
          getNumWays :: Float -> Float -> Int
          getNumWays t d = max 0 (ceiling num1 - floor num2 - 1)
            where
              num1 = (t + sqrt (t * t - (4*d)))/2.0
              num2 = (t - sqrt (t * t - (4*d)))/2.0
part2 :: IO ()
part2 = do
  input <- readFile "textFiles/day6"
  print doP2
    where
      doP2 :: Integer
      doP2 = getNumWays 71530 940200 -- 46689866 358105418071080
        where
          getNumWays :: Integer -> Integer -> Integer
          getNumWays t d =max 0 (ceiling num1 - floor num2 - 1)
            where
              num1 = (tplus)/2
              num2 = (tminus)/2
              discriminant :: Integer
              discriminant = t*t - (d*d)
              sqrtdiscriminant :: Float
              sqrtdiscriminant = sqrt (fromInteger discriminant)
              tplus :: Float
              tplus  =(fromInteger t) + sqrtdiscriminant
              tminus :: Float
              tminus =(fromInteger t) - sqrtdiscriminant
