{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Gloss.Scene
import Reflex.Gloss.Random
import Reflex.Monad.Time
import Reflex.Animation

import Graphics.Gloss

import Data.Monoid
import Data.Maybe

import qualified Data.Map as Map
import Control.Applicative hiding (optional)

import Widgets

holdDirection :: Reflex t => KeyEvents t -> Scene t (Behavior t Float)
holdDirection keys = do
  left  <- holdKey KeyLeft
  right <- holdKey KeyRight
  
  return (liftA2 (+) (negate <$> left) right)

  where holdKey s = hold 0.0 $ leftmost 
          [ 1.0 <$ specialDown keys s
          , 0.0 <$ specialUp keys s
          ]

          
pong :: Reflex t => Vector -> Scene t ()
pong (width, height) = do
  keys <- keyEvents
  paddleDir <- holdDirection keys
  
  x <- integrate 0 ((paddleSpeed *) <$> paddleDir) 
  render (paddle <$> x)

  return ()
    where
  
      paddleSpeed = 160.0

      paddle x = translate x (bottom + 20) $
        color red $ rectangleSolid 100 10
        
      bottom = -height/2  
      bounds = (-width/2, height/2)


main = playSceneGraph display background frequency $ 
  pong (fromIntegral width, fromIntegral height)
    where 
      display = InWindow "Pong1" (width, height) (0, 0)
      background = white
      frequency = 30
      width = 600
      height = 600
      