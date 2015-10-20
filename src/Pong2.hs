{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Gloss.Scene
import Reflex.Gloss.Random
import Reflex.Monad.Time
import Reflex.Animation

import Graphics.Gloss

import Control.Monad
import Data.Monoid
import Data.Maybe

import qualified Data.Map as Map
import Control.Applicative hiding (optional)

import Widgets

radius :: Float
radius = 6


          
pong :: Reflex t => Vector -> Scene t ()
pong (width, height) = mdo

  paddlePos <- fmap fst <$> localMouse  
  initial <- sample (onPaddle <$> paddlePos)

  ballVel <- hold (200, 200) never
  ballPos <- integrate initial ballVel
  

  render $ mconcat 
    [ drawBall <$> current ballPos
    , drawPaddle <$> paddlePos
    ]
    
    where
      
      onPaddle pos = (pos, bottom + radius)
     
      drawBall (x, y) = translate x y $ 
        color green $ circleSolid radius
      
      drawPaddle pos = translate pos (bottom - thickness/2) $
        color red $ rectangleSolid 100 thickness
        
      thickness = 10
      bottom = -height/2 + 20


main = playSceneGraph display background frequency $ 
  pong (fromIntegral width, fromIntegral height)
    where 
      display = InWindow "Pong2" (width, height) (0, 0)
      background = white
      frequency = 30
      width = 600
      height = 600
      