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

  
overPaddle :: Float -> Point -> Bool
overPaddle pos (x, _) = x - pos < paddleWidth/2 && x - pos > -paddleWidth/2

  
collidePaddle :: Float -> Float -> Point -> Maybe Vector
collidePaddle bottom pos (x, y) 
  | y <= bottom && overPaddle pos (x, y)  = Just (0, -1)
  | otherwise = Nothing
  
  
hasMissed :: Float -> Float -> Point -> Maybe ()
hasMissed bottom pos (x, y) 
  | y <= bottom && not (overPaddle pos (x, y))  = Just ()
  | otherwise = Nothing
  
  

-- Ball hit the floor
hitFloor :: Vector -> Point -> Bool
hitFloor (width, height) (_, y) =  y - radius < -height / 2


-- reflect a vector about a normal
reflect :: Vector -> Vector -> Vector
reflect n v = v  ^-^  (2 * (n <.> v)) *^ n  


    
          
pong :: Reflex t => Vector -> Scene t ()
pong (width, height) = do
  window <- targetWindow

  paddlePos <- fmap fst <$> localMouse
  
  let 
    stopped = Workflow $ return
      ((never, onPaddle <$> paddlePos)
      , bouncing <$ mouseDown LeftButton window)
     
    bouncing = Workflow $ mdo
      initial <- sample (onPaddle <$> paddlePos)
      ballVel <- foldDyn reflect (200, 200) (leftmost [hitWall, hitPaddle])
      ballPos <- integrate initial (current ballVel)
  
      let hitWall   = fmapMaybe (collideWalls (width, height)) (updated ballPos)
          hitPaddle = attachWithMaybe (collidePaddle bottom) paddlePos (updated ballPos)    
          missedPaddle = attachWithMaybe (hasMissed bottom) paddlePos (updated ballPos)
          
      return ((missedPaddle, current ballPos), stopped <$ missedPaddle)
  
  (loss, ballPos) <- splitDyn =<< workflow stopped
  died <- count (switchPromptlyDyn loss) 
  

  
  render $ mconcat 
    [ drawBall <$> join (current ballPos)
    , drawPaddle <$> paddlePos
    , drawLives . (initialLives - ) <$> current died  
    ]
    
    where
      
      onPaddle pos = (pos, bottom + radius)
      radius = 6
      
      drawBall (x, y) = translate x y $ 
        color green $ circleSolid radius
      
      drawPaddle pos = translate pos (bottom - thickness/2) $
        color red $ rectangleSolid 100 thickness
        
      drawLives lives = translate 250 250 $ scale 0.2 0.2 $ text (show lives)
      
      initialLives = 5
      thickness = 10
      bottom = -height/2 + 20


main = playSceneGraph display background frequency $ 
  pong (fromIntegral width, fromIntegral height)
    where 
      display = InWindow "Pong4" (width, height) (0, 0)
      background = white
      frequency = 30
      width = 600
      height = 600
      