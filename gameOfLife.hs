-- ### QUESTION 2.1 ###
-- Takes in a list of list of strings and pretty-print a possibly-infinite sequence of strings.
pretty :: [[String]] -> String
pretty xs = unlines $ concat $ xs

--------------------------------------------------------------------------------
-- ### QUESTION 2.2 ###
type Point = (Int,Int)

glider :: [Point]
glider = [(0,2),(1,3),(2,1),(2,2),(2,3)]

-- Creates an empty grid consisting out of a String of dots.
emptyGrid :: Int -> Int -> [String]
emptyGrid x y = replicate (y+1) (replicate (x+1) '.')

-- Replaces one '.' with a '#' at the position mentioned in the 'a' in a string.
replaceInString :: String -> Int -> String
replaceInString [x] a 
  | a > 0 = [x] 
  | otherwise = "#" 
replaceInString (x:xs) a 
  | a > 0 = x : replaceInString xs (a-1)
  | a == 0 = '#' : xs

-- Replaces one '.' with a '#' at the position of the given Point in a list of strings.
replaceInListOfStr :: [String] -> Point -> [String]
replaceInListOfStr [x] (a,b) 
  | b > 0 = [x] 
  | otherwise = [replaceInString x a]
replaceInListOfStr (x:grid) (a,b) 
  | b > 0 = x : replaceInListOfStr grid (a,(b-1)) 
  | b == 0 = (replaceInString x a) : grid

-- Replaces the '.' with '#' on the positions of the 'glider'/set of points.
replaceElements :: [String] -> [Point] -> [String]
replaceElements grid [] = grid
replaceElements grid [x] = replaceInListOfStr grid x
replaceElements grid (x:xs) = replaceElements (replaceInListOfStr grid x) xs

-- Visualize one grid (returns visualization as a list of strings).
partVisualisation :: [String] -> [[Point]] -> [[String]]
partVisualisation grid [] = [grid]
partVisualisation grid [p] = [replaceElements grid p]
partVisualisation grid (p:ps) = replaceElements grid p : partVisualisation grid ps

-- Visulises a sequence of generations by implementing emptyGrid system.
visualisation :: Int -> Int -> [[Point]] -> [[String]]
visualisation x y p = partVisualisation (emptyGrid x y) p

--------------------------------------------------------------------------------
-- ### QUESTION 2.3 ###
-- Checks if two Points are neighbours by checking for both, if their x and y value just differ by 1 but equal Points should evalute to False
areNeighbours :: Point -> Point -> Bool
areNeighbours (a,b) (c,d) 
  | abs (a - c) == 1 && abs (b - d) <= 1 = True
  | abs (a - c) <= 1 && abs (b - d) == 1 = True
  | otherwise = False

-- Determines how many neighbours a cell has by giving it a list of points. If there are 3 alive next to it then returns true as alive.
isAlive3 :: Point -> [Point] -> Bool
isAlive3 p ps = (length (filter (areNeighbours p) ps)) == 3

--- Determines how many neighbours a cell has by giving it a list of points. If there are 2 alive next to it then returns true as alive.
isAlive2 :: Point -> [Point] -> Bool
isAlive2 p ps = (length (filter (areNeighbours p) ps)) == 2

-- Takes in a list of points and returns a list which contains all possible neighbours of a cell in the grid.
possibleNeighbours :: [Point] -> [Point]
possibleNeighbours [] = []
possibleNeighbours ((x,y):ps) 
  -- returns the 8 possible neighbours for middle part of grid
  | x > 0 && y > 0 = (x-1,y-1) : (x,y-1) : (x-1,y) : (x+1,y) : (x,y+1) : (x+1,y-1) : (x-1,y+1) : (x+1,y+1) : possibleNeighbours ps
  -- returns the 5 possible neighbours for left edge points on grid
  | x > 0 && y == 0 = (x-1,y) : (x+1,y) : (x,y+1) : (x-1,y+1) : (x+1,y+1) : possibleNeighbours ps 
  -- returns the 5 possible neighbours for top edge points on grid
  | y > 0 && x == 0 = (x,y-1) : (x+1,y) : (x,y+1) : (x+1,y-1) : (x+1,y+1) : possibleNeighbours ps
  -- returns 3 possible neighbours for corner points
  | otherwise = (x+1,y) : (x,y+1) : (x+1,y+1) : possibleNeighbours ps

-- Removes duplicate Points in a list (to remove duplicates from possibleNeighbours)
removeDuplicates :: [Point] -> [Point]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : (removeDuplicates (remove x xs))
  where
    remove :: Point -> [Point] -> [Point]
    remove x [] = []
    remove x (y:ys)
      | x == y = remove x ys
      | otherwise = y : (remove x ys)

-- Makes one evolution step by putting all existing cells with more than 2 cells and all dead cells wich have 3 neighbours in the next evolution step
oneEvolutionStep :: [Point] -> [Point]
oneEvolutionStep ps 
  = removeDuplicates $ filter (\x -> isAlive3 x ps || isAlive2 x ps) ps ++ filter (\x -> isAlive3 x ps) (possibleNeighbours ps)
    
-- Create potential infinite list of evolutions by applying oneEvolutionStep recursively and appending to list.
evolution :: [Point] -> [[Point]]
evolution = iterate oneEvolutionStep

--------------------------------------------------------------------------------
-- ### MAIN FUNCTION ###
main :: IO ()
main = putStrLn (pretty (take 8 (visualisation 5 5 (evolution glider))))

-- Check all functions individually
-- main = putStrLn(show(visualisation 5 5 [ glider ]))
-- main = putStrLn(show(pretty [[['a','b'],['c','d']],[['e','f'],['g','h']],[['i','j'],['k','l']]]))
