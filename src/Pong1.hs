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

          
pong :: Reflex t =>  Vector -> Scene t ()
pong (width, height) = do

  paddlePos <- fmap fst <$> localMouse
  render (drawPaddle <$> paddlePos)

    where
  
      drawPaddle pos = translate pos (bottom - thickness/2) $
        color red $ rectangleSolid 100 thickness
        
      thickness = 10       
      bottom = -height/2 + 20
  

main = playSceneGraph display background frequency $ 
  pong (fromIntegral width, fromIntegral height)
    where 
      display = InWindow "Pong1" (width, height) (0, 0)
      background = white
      frequency = 30
      width = 600
      height = 600
      