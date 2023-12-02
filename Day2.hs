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
            getLineVal :: String -> Int --0 or ID value
            getLineVal gl
                   | all checkRound (splitOn "; " rest) = id
                   | otherwise                         = 0
              where
                rest :: String
                ['G':'a':'m':'e':num,rest] = splitOn ": " gl
                id = read num
                checkRound :: String -> Bool
                checkRound round = all checkCubePairs (joinNumColour (filter (not.null) (splitOneOf ", " round)))
                  where
                    joinNumColour :: [String] -> [(Int, Char)]
                    joinNumColour [] = []
                    joinNumColour (a:(b:_):s) = (read a, b) : joinNumColour s
                    checkCubePairs :: (Int, Char) -> Bool
                    checkCubePairs (n, 'r') = n <= 12
                    checkCubePairs (n, 'g') = n <= 13
                    checkCubePairs (n, 'b') = n <= 14
