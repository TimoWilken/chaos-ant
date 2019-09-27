module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Foldable
import Text.Printf

import Graphics.UI.Gtk

data CardinalDirection = North | East | South | West deriving (Show, Eq)

instance Enum CardinalDirection where
  succ = toEnum . succ . fromEnum
  pred = toEnum . pred . fromEnum

  fromEnum North = 0
  fromEnum East = 1
  fromEnum South = 2
  fromEnum West = 3

  toEnum 0 = North
  toEnum 1 = East
  toEnum 2 = South
  toEnum 3 = West
  toEnum i = toEnum $ i `mod` 4

data Position a = Position a a deriving (Eq, Show)

getX :: Position a -> a
getX (Position x _) = x
getY :: Position a -> a
getY (Position _ y) = y

walk :: Enum a => CardinalDirection -> Position a -> Position a
walk North (Position x y) = Position x (succ y)
walk East (Position x y) = Position (succ x) y
walk South (Position x y) = Position x (pred y)
walk West (Position x y) = Position (pred x) y

data AntGrid a = AntGrid { blackSquares :: [Position a],
                           antPosition :: Position a,
                           antFacing :: CardinalDirection }

startGrid :: Num a => AntGrid a
startGrid = AntGrid { blackSquares = mempty, antPosition = Position 0 0,
                      antFacing = North }

squareIsBlack :: Eq a => Position a -> AntGrid a -> Bool
squareIsBlack pos = elem pos . blackSquares

antStep :: (Eq a, Enum a) => AntGrid a -> AntGrid a
antStep grid = AntGrid {
  blackSquares = (if onBlack then delete else (:)) pos $ blackSquares grid,
  antPosition = walk newFacing pos, antFacing = newFacing }
  where newFacing = (if onBlack then succ else pred) $ antFacing grid
        onBlack = squareIsBlack pos grid
        pos = antPosition grid

data BBox a = BBox a a a a

instance Eq a => Eq (BBox a) where
  (==) (BBox x1 y1 w1 h1) (BBox x2 y2 w2 h2) =
    x1 == x2 && y1 == y2 && w1 == w2 && h1 == h2

bbox :: (Ord a, Num a) => AntGrid a -> BBox a
bbox grid
  | null $ blackSquares grid = BBox 0 0 0 0
  | otherwise = BBox (minimum xs) (minimum ys) (maximum xs) (maximum ys)
  where xs = map getX $ blackSquares grid
        ys = map getY $ blackSquares grid

instance (Ord a, Enum a, Num a) => Show (AntGrid a) where
  show grid = intercalate "\n" rows
    where (BBox x0 y0 x1 y1) = bbox grid
          rows = [[squareChar (Position x y) grid | x <- [x0..x1]] | y <- [y0..y1]]

squareChar pos grid | antPosition grid == pos = if onBlack then '@' else ','
                    | otherwise = if onBlack then '#' else ' '
  where onBlack = squareIsBlack pos grid

stepDifference :: (Ord a, Eq a, Enum a, Num a) => AntGrid a -> (AntGrid a, Position a, Bool)
stepDifference grid = (nextGrid, antPosition grid, bbox grid /= bbox nextGrid)
  where nextGrid = antStep grid

updateScreen :: (Ord a, Show a, Eq a, Enum a, Num a, PrintfArg a) => AntGrid a -> IO ()
updateScreen grid = putStrLn $ case stepDifference grid of
  (nextGrid, _, True) -> "\ESC[1;1H" ++ show nextGrid  -- print at screen origin
  (nextGrid, changedPos@(Position x y), False) ->
    let (BBox x0 y0 _ _) = bbox nextGrid
    in printf "\ESC[%d;%dH%c"
       (y - y0 + 1) (x - x0 + 1) (squareChar changedPos nextGrid)

textMain :: IO ()
textMain = do
  putStr "\ESC[2J"  -- clear the screen
  traverse_ updateScreen $ iterate antStep (startGrid :: AntGrid Int)

guiMain :: IO ()
guiMain = do
  void initGUI
  window <- windowNew
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False
  window `set` [windowTitle := "Chaos Ant",
                windowResizable := True,
                windowDefaultWidth := 640,
                windowDefaultHeight := 640]
  canvas <- drawingAreaNew
  window `containerAdd` canvas
  widgetModifyBg canvas StateNormal (Color 0xffff 0 0)
  widgetShowAll window
  mainGUI

main :: IO ()
main = textMain
