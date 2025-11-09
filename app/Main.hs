module Main where

import qualified Graphics.Gloss as G
import Graphics.Gloss.Interface.IO.Game (Display(InWindow), playIO, black)
import System.Random (newStdGen)

import Lib
  ( initialWorld
  , drawWorldIO, handleInputIO, stepWorldIO
  , readScoresIO, top3, World(..)
  )

window :: G.Display
window = InWindow "Shoot 'em up — typeclasses stap" (1280, 800) (100, 100)

background :: G.Color
background = black

fps :: Int
fps = 60

main :: IO ()
main = do
  g <- newStdGen
  scores <- readScoresIO
  let w0 = (initialWorld g) { highscores = top3 scores }
  playIO window background fps w0 drawWorldIO handleInputIO stepWorldIO
