-- Author:      Jakub Vlk (xvlkja07)
-- Year:        2024/2025
--
-- Description:

import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (dropWhileEnd, find, isPrefixOf, nub, partition,minimumBy)
-- import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Data.Ord (comparing)


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim l =
  let prefix = takeWhile (/= delim) l
      rest   = drop 1 (dropWhile (/= delim) l)
  in prefix : splitOn delim rest



-- Simple tree definition
data Tree a b = Lf b | Nd a (Tree a b) (Tree a b)
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("-1" : treeFile : dataFile : []) -> do
      -- putStrLn $ "Režim -1: načítám strom ze souboru " ++ treeFile ++ " a data ze souboru " ++ dataFile
      treeString <- readFile treeFile
      dataFileStr <- readFile dataFile
      --             STRINGING FROM ARRAY                                                TREEE                                    CLASES VALUES
      --        ________________________________                   ______________________________________________________     _________________________
      putStrLn (foldl (\x y -> x++(y)++"\n" ) ""  (map (findClass (parseTreeLines (map countIndent (lines treeString)) 0) ) (parseClassString $ lines dataFileStr)))
    ("-2" : trainFile : []) -> do
      trainFileStr <- readFile trainFile
      printTree $ buildTree $ (map (\x -> loadTrainFile x ) (lines trainFileStr))
      -- putStrLn (show )
      -- putStrLn $ "Režim -2: načítám trénovací data ze souboru " ++ trainFile
    ("-3" : treeFile : []) -> do
      treeString <- readFile treeFile
      putStrLn (show (parseTreeLines (map countIndent (lines treeString)) 0))
    ("-4" : trainFile : []) -> do
      trainFile <- readFile trainFile
      putStrLn (show (map (\x -> loadTrainFile x ) (lines trainFile)) )

    _ -> putStrLn "Použití: flp-fun -1 <tree_file> <data_file> \n flp-fun -2 <train_file> \n flp-fun -3 <tree_file>"


-- Creates list of pair where first element is deepth of element and second is string without beggined spaces
-- countIndent :: [String] -> [(Int, String)]
countIndent x = (((length . takeWhile isSpace) x) `div` 2, dropWhile (not . isAlpha) x) -- pair of (inherence, line without spaces)


-- isNext line line with leaf
isLeaf x
  | "Leaf:" `isPrefixOf` x = True
  | otherwise = False


-- Function like takeWhile but it will keeps the boundary element.
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x : xs) =
  x
    : if p x
      then takeWhileInclusive p xs
      else []

-- This function takes a list of pairs (X, l), where:
--   - X is a list of values, with the first element representing the expected depth of that element and second is the string repsesented line wihtout begining lines.
--   - l is the current level (the first call should be made with zero).
--
-- !!! WARNING: This function does not perform ANY validity checks !!!
--
-- How it works:
--
-- Essentially, the function transforms X into a list of depth numbers (parsing of the text is handled later in the code by calling another function).
-- For example, consider the list:
--
--   [1, 2, 3, 3, 2, 3, 3, 4, 4, 5, 5]
--
-- In this list, the first element is extracted and used for creating the current node. Then:
--
-- a) For the left subtree, we are interested in all elements up to (and including) current level + 1 (i.e. l + 1). In this example, that would be:
--      [2, 3, 3, 2]
--    - This is done by reversing the list and removing elements until you reach the element corresponding to l + 1 (in this case, the element 2).
--    - The remaining code handles pairing these elements appropriately.
--
-- b) For the right subtree, we take all elements from right to left until we meet an element equal to l + 1 (including that element), for example:
--      [2, 3, 3, 4, 4, 5, 5]
--    - A custom function, takeWhileInclusive, is used here; it works like takeWhile but also keeps the boundary element.
--
-- a+) If the upcoming left element represents a leaf, we must use a different approach:
--    We split the list so that the first element is used for the left node and the rest of the list (without the first element) is used for the right node.
--    For example, given [2, 3, 3, 4, 4, 5, 5]:
--    - The value 2 is used for the current node.
--    - Then the first 3 is clearly a leaf node (with no further nodes), so the left subtree will use [3] while the right subtree uses [3, 4, 4, 5, 5].
--
-- For parsing the actual values, the functions parseNodeLine and parseNodeLf are used, respectively.
parseTreeLines ((y, x) : xz) l
  | "Leaf:" `isPrefixOf` x = Lf (parseNodeLf x)
  | "Node:" `isPrefixOf` x && isLeaf ((\((a, b) : _) -> b) xz) = Nd (parseNodeLine x) (parseTreeLines (take 1 xz) (l + 1)) (parseTreeLines (drop 1 xz) (l + 1))
  | "Node:" `isPrefixOf` x = Nd (parseNodeLine x) (parseTreeLines (reverse (dropWhile (\(a, b) -> a /= (l + 1)) (reverse xz))) (l + 1)) (parseTreeLines (reverse ((takeWhileInclusive (\(a, b) -> a /= (l + 1)) (reverse xz)))) (l + 1))


-- takes string as an argument an extract the index of feature and edge value
parseNodeLine :: String -> (Int, Float)
parseNodeLine x = (\[a, b] -> (read a :: Int, read b :: Float)) (splitOn ',' (dropWhile (not . isDigit) x))

-- From line contains the Leaf extracet just the name of the class
parseNodeLf :: String -> String
parseNodeLf x = (\[a, b] -> b) (splitOn ' ' x)

------------------------------ parsing file with values and classfication of values :)------------------------------



-- parse the string of class (leaf node)
parseClassString :: [String] -> [[Float]]
parseClassString x = map (\y -> map (read) (splitOn ',' y)) x


 -- base on input and tree returns input :(
findClass :: Ord a => Tree (Int, a) b -> [a] -> b
findClass t val = case t of
  Lf x      -> x
  Nd (a,b) l r  -> if (val !! a) < b then findClass l val else findClass r val


------------------------------------------------------ cart algorithm impl ------------------------------------------------------------------


--- parse cartfile

loadTrainFile :: String -> ([Float],String)
loadTrainFile x = ( map (\y -> read y :: Float)  ( reverse (drop 1 (reverse (splitOn ',' x)))) , last (splitOn ',' x))


-- Takes the list of sambles in format [([Float],String) ] and creates the list of strings that contains all of the mentioned class once.
-- Params: 
-- Samples, forward propagation
trainedClass [] v = v
trainedClass ((_, xb):xs) v = if xb `elem` v then  trainedClass xs v else trainedClass xs (xb:v)  

-- Takes the list of sambles in format [([Float],String) ] and return how manytime is in the class used.
countClassN l className = foldl (\x y -> if (snd y) == className then (1+x) else x ) 0 l


-- gimi (trainedClass traintest []) traintest 
gimi samples = 1- sum (map (\x -> (fromIntegral (countClassN samples x) / fromIntegral (length samples))^2) (trainedClass samples []))


-- helper for computing weight gimi index
gimiWeight samples = (gimi samples)* fromIntegral (length samples)


-- takes the samples and creates the list of possible spliting options
getListOfBorders samples x_num = map (\x -> x !! x_num)$ map (\x -> fst x ) samples


-- split samples to 2 groups base on feature index and Border value
splitDataByBorder x_samples y_Borderd z_index = partition (\x -> ((fst x) !! z_index) < y_Borderd) x_samples

-- Gimi of splited 
gimiOfSplit (x,y) = [gimiWeight x, gimiWeight y ]



-- tuple (param index, GIMI, split border)
getSplit samples a = map (\x -> (a, (sum $ gimiOfSplit $  splitDataByBorder samples x a ) / fromIntegral (length samples), x) ) $ getListOfBorders samples a

-- splits the samples by each feature and every option
-- out: [(param index, GIMI, split border)]

getAllSplits samples x
  | x < 0     = []
  | otherwise = getSplit samples x ++ getAllSplits samples (x - 1)


countNumberOfParams samples = length $ fst (samples !! 0) 


-- Returns minimal split value and param number based on minimal gimi index, (paramid, gimi, split value )
getMinimalSplit samples =
    minimumBy (comparing (\(_, c, _) -> c)) $
      concatMap (getSplit samples) [0..countNumberOfParams samples - 1]

-- Test if there is only one class 
shouldStop samples = if ((length $ trainedClass samples []) < 2) then True else False

-- Create the representation of the Leaf node
getLabel samples = let [x] = trainedClass samples []
                    in x


-- Create the tree from samples
buildTree samples
  | shouldStop samples = Lf (getLabel samples)
  | otherwise =
      let (paramId, _ , splitValue) = getMinimalSplit samples
          (leftSamples, rightSamples) = splitDataByBorder samples splitValue paramId
      in Nd (paramId, splitValue) (buildTree leftSamples) (buildTree rightSamples)


-- wrapper for PrintTreeHelper Function, Print the treen in expected format
printTree :: (Show x, Show y, Show b) => Tree (x, y) b -> IO ()
printTree tree = printTreeHelper tree 0

printTreeHelper
  :: (Show x, Show y, Show b) => Tree (x, y) b -> Int -> IO ()
printTreeHelper (Lf b) indent =
  putStrLn $ replicate indent ' ' ++ "Leaf: " ++ show b

printTreeHelper (Nd (x, y) left right) indent = do
  putStrLn $ replicate indent ' ' ++ "Node: " ++ show x ++ ", " ++ show y
  printTreeHelper left  (indent + 2)
  printTreeHelper right (indent + 2)