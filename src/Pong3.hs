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
import Data.VectorSpace

import Debug.Trace

import qualified Data.Map as Map
import Control.Applicative hiding (optional)

import Widgets

radius :: Float
radius = 6

paddleWidth :: Float
paddleWidth = 100


-- return the collision normal
collideWalls :: Vector -> Point -> Maybe Vector
collideWalls (width, height) (x, y) 
    | x - radius <=  -width  / 2 = Just (1, 0)  -- Hit left wall
    | x + radius >=   width  / 2 = Just (-1, 0) -- Hit right wall
    | y - radius >=   height / 2 = Just (0, 1)  -- Hit the roof
    | otherwise = Nothing -- No collision

  
  
collidePaddle :: Float -> Float -> Point -> Maybe Vector
collidePaddle bottom pos (x, y) 
  | y <= bottom && x - pos < paddleWidth/2 && x - pos > -paddleWidth/2 = Just (0, -1)
  | otherwise = Nothing

-- Ball hit the floor
hitFloor :: Vector -> Point -> Bool
hitFloor (width, height) (_, y) =  y - radius < -height / 2


-- reflect a vector about a normal
reflect :: Vector -> Vector -> Vector
reflect n v = v  ^-^  (2 * (n <.> v)) *^ n  
  
          
pong :: Reflex t => Vector -> Scene t ()
pong (width, height) = mdo

  paddlePos <- fmap fst <$> localMouse
  
  initial <- sample (onPaddle <$> paddlePos)
  ballVel <- foldDyn reflect (200, 200) (leftmost [hitWall, hitPaddle])
  
  ballPos <- integrate initial (current ballVel)
  
  let hitWall   = fmapMaybe (collideWalls (width, height)) (updated ballPos)
      hitPaddle = attachWithMaybe (collidePaddle bottom) paddlePos (updated ballPos)  
  
  render $ mconcat 
    [ drawBall <$> current ballPos
    , drawPaddle <$> paddlePos
    ]
    
    where
      
      onPaddle pos = (pos, bottom + radius)
     
      drawBall (x, y) = translate x y $ 
        color green $ circleSolid radius
      
      drawPaddle pos = translate pos (bottom - thickness/2) $
        color red $ rectangleSolid paddleWidth thickness
        
      thickness = 10
      bottom = -height/2 + 20


main = playSceneGraph display background frequency $ 
  pong (fromIntegral width, fromIntegral height)
    where 
      display = InWindow "Pong3" (width, height) (0, 0)
      background = white
      frequency = 30
      width = 600
      height = 600
      