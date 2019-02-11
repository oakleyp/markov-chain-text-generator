import Data.Map
import Test.QuickCheck (generate, elements)

randItem :: [a] -> IO a
randItem = generate . elements

takeRange :: [Int] -> [a] -> [a]
takeRange range inlist  = [inlist !! x | x <- range]

processMap :: [(String, String)] -> [(String, [String])]
processMap [] = []
processMap (firstPair:inlist) =
  [ (prefix, [suffix] ++ [snd (inlist !! x) | x <- [0..listEnd], (fst (inlist !! x)) == prefix ]) ]
   ++ (processMap $ removeSeen inlist)
  where {
    listEnd = (length inlist) - 1
    ; prefix = (fst firstPair)
    ; suffix = (snd firstPair)
    ; removeSeen inlist = [inlist !! x | x <- [0..listEnd], (fst (inlist !! x)) /= prefix]
  }

buildMap :: [(String, String)] -> Map String [String]
buildMap inlist = fromList (processMap inlist)

wordsToDict :: [String] -> Int -> [(String, String)]
wordsToDict wordList preLen
  | (length wordList == 0 || preLen == 0) = []
  | preLen > ((length wordList) - 1) = wordsToDict wordList (preLen - 1)
  | otherwise = wordsToDict' wordList preLen

wordsToDict' :: [String] -> Int -> [(String, String)]
wordsToDict' wordList preLen =
   [(
    (unwords prefix), 
    (wordList !! (preLen)) 
   )] ++ (wordsToDict (takeRange [(preLen+1)..listEnd] wordList) preLen) 
  where {
    listEnd = (length wordList) - 1
    ; prefix = if preLen == 1 then [wordList !! 0] else (takeRange [0..preLen-1] wordList)
  }

dictToString :: [String] -> Map String [String] -> Int -> Int -> IO String
dictToString origText dict n 0 = return ""
dictToString origText dict n max 
  | (length origText < n) = return ""
  | otherwise = dictToString' origText dict n max 

dictToString' :: [String] -> Map String [String] -> Int -> Int -> IO String
dictToString' origText dict n max = do
  np <- newPrefix
  suffix <- (dictToString (takeRange [n+1..listEnd] origText) dict n (max-n))
  return $ rotor ++ " " ++ np ++ " " ++ suffix
  where {
    rotor = unwords (takeRange [0..n-1] origText)
    ; newPrefix = randItem (findWithDefault [""] rotor dict)
    ; listEnd = (length origText) - 1
  }

main :: IO ()
main = do
  let maxW = 300
  let n = 3
  inputFile <- getLine >>= readFile

  let wordsIn = words inputFile
  let wordDict = wordsToDict wordsIn n
  let wordMap = buildMap wordDict

  -- print wordDict
  -- print $ processMap wordDict
  -- print $ wordMap
  result <- dictToString wordsIn wordMap n maxW
  putStrLn result
