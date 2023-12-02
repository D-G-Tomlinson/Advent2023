import System.IO
import Data.List.Split

part1 :: IO ()
part1 = do
  answer <- doP1
  print answer
    where
      doP1 :: IO Int
      doP1 = do
        fileText <- readFile "textFiles/day2"
        return (foldl (\a b -> a + getLineVal b) 0 (lines fileText))
          where
            getLineVal :: String -> Int
            getLineVal gl
                   | all checkRound (splitOn "; " rest) = id
                   | otherwise                          = 0
              where
                ['G':'a':'m':'e':num,rest] = splitOn ": " gl
                id = read num
                checkRound :: String -> Bool
                checkRound round = all checkCubePairs (joinNumColour (filter (not.null) (splitOneOf ", " round)))
                  where
                    joinNumColour :: [String] -> [(Int, Char)]
                    joinNumColour []          = []
                    joinNumColour (a:(b:_):s) = (read a, b) : joinNumColour s
                    checkCubePairs :: (Int, Char) -> Bool
                    checkCubePairs (n, 'r') = n <= 12
                    checkCubePairs (n, 'g') = n <= 13
                    checkCubePairs (n, 'b') = n <= 14
part2 :: IO ()
part2 = do
  answer <- doP2
  print answer
   where
     doP2 :: IO Int
     doP2 = do
        fileText <- readFile "textFiles/day2"
        return (foldl (\a b -> a + getLineVal b) 0 (lines fileText))
          where
            getLineVal :: String -> Int 
            getLineVal gl = r * g *b
              where
                [_,consider] = splitOn ": " gl
                pairs        = joinNumColour (filter (not.null) (splitOneOf ", " consider))
                  where
                    joinNumColour :: [String] -> [(Int, Char)]
                    joinNumColour []          = []
                    joinNumColour (a:(b:_):s) = (read a, b) : joinNumColour s
                (r,g,b) = foldl checkMax (0, 0, 0) pairs
                  where
                    checkMax :: (Int, Int, Int) -> (Int, Char) -> (Int, Int, Int)
                    checkMax (r,g,b) (n, 'r') = (max r n, g, b)
                    checkMax (r,g,b) (n, 'g') = (r, max g n, b)
                    checkMax (r,g,b) (n, 'b') = (r, g, max b n)
              
