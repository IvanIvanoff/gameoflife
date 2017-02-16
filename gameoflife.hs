import qualified Data.Text as Text
import Control.Concurrent
import System.Console.ANSI

data State = Alive|Dead deriving (Eq)
type Cell = ((Int, Int), State)
type Coordinate = (Int,Int)
type Grid = [Cell]

-- Globals. Set according to my 13.3" screen
width :: Int
width = 50

height :: Int
height = 35

-- Initiates the grid with all states equal to Dead except the passed ones
initGrid :: [Coordinate] -> Grid
initGrid coords = [((x,y), determineState x y) | x <- [0..height], y <- [0..width]]
    where determineState x y
           | (x,y) `elem` coords = Alive
           | otherwise           = Dead

-- Get the cell of the given position
getCellAt :: Coordinate -> Grid -> Cell
getCellAt (x,y) grid =
    grid !! (x * (1 + width) + y)

-- Get the state of the given position
getStateAt :: Coordinate -> Grid -> State
getStateAt (pos) grid = state
    where ((_,_),state) = getCellAt pos grid

-- All neighbours withing the bounds
neighbours :: Coordinate -> [Coordinate]
neighbours (x, y) = filter inBound [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]
    where inBound = (\(u,v) -> (u,v) /= (x,y) && u >= 0 && v >= 0 && u <= height && v <= width)

-- Determine next state of a cell
nextCellState :: Cell -> Grid -> State
nextCellState ((x,y),state) grid
    | 2 == countAlive && Alive == state = Alive
    | 3 == countAlive                   = Alive
    | otherwise                         = Dead
    where countAlive = (length . filterAlive . neighbours) (x,y)
          filterAlive = filter (\pos -> Alive == (getStateAt pos grid))

-- Evolve the grid with one generation
evolve :: Grid -> Grid
evolve grid = map nextCell grid
    where nextCell = (\(pos,state) -> (pos, (nextCellState (pos,state) grid)))

-- Transform the grid to a string
toString :: Grid -> String
toString grid = Text.unpack $ toStringHelper grid

toStringHelper :: Grid -> Text.Text
toStringHelper grid = Text.intercalate (Text.singleton '\n') (rows 0)
  where
    rows x
      | x > height = []
      | otherwise = (row x 0) : rows (x + 1)
    row x y
      | y > width = Text.empty
      | otherwise = Text.cons (stateToChar $ getStateAt (x,y) grid) (row x (y + 1))
    stateToChar state
      | state == Alive = '@'
      | otherwise = '.'

-- Draw the grid in the terminal
draw :: Grid -> IO()
draw grid = do
  let updatedGrid = evolve grid
  clearScreen
  putStr $ toString updatedGrid
  putStr "\n\n"
 -- threadDelay 100000 -- use this if you are using ghci and calling draw from there
  draw updatedGrid

-- Different grids
glider :: Grid
glider = initGrid [(8,17),(8,18),(8,19),(9,17),(10,18)]

beacon :: Grid
beacon = initGrid [(5,5),(5,6),(6,5),(6,6),(7,7),(7,8),(8,7),(8,8)]

toad :: Grid
toad = initGrid [(5,5),(5,6),(5,7),(6,6),(6,7),(6,8)]

gliderGun :: Grid -- spawns gliders. hooray
gliderGun = initGrid [(7,3),(8,3),(7,4),(8,4),(7,13),(8,13),(9,13),(6,14),(10,14),(5,15),(11,15),(5,16),(11,16),(8,17),(6,18),(10,18),(7,19),(8,19),(9,19),(8,20),(5,23),(6,23),(7,23),(5,24),(6,24),(7,24),(4,25),(8,25),(3,27),(4,27),(8,27),(9,27),(5,37),(6,37),(5,38),(6,38)]

main :: IO()
main = do
    draw gliderGun
