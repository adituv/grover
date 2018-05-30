{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Vector as V
import Graphics.Gloss
import Grover
import qualified Linear as L
import System.IO

main :: IO ()
main = do
  n <- prompt "How many qubits?"
  a <- prompt "Solution?"
  simulate
    (InWindow "Grover's Search Algorithm" (600,600) (10,10))
    white
    2
    (initState (mkTarget n a))
    displayState
    (\_ _ -> updateState)

prompt :: Read a => String -> IO a
prompt str = do
  putStrLn str
  putStr "> "
  hFlush stdout
  readLn

updateState :: GroverState -> GroverState
updateState state@GroverState{..}
  | abs (getAngle target current) < theta = state
  | otherwise = nextState state
  where
    theta = abs (90 - getAngle target psi)

displayState :: GroverState -> Picture
displayState GroverState{..} =
  pictures
    [ graphAxes
    , vectorAtAngle 0 "a"  -- Put target always vertical
    , vectorAtAngle (getAngle target psi) "psi"
    , color red $ vectorAtAngle (getAngle target current) "x"
    ]

rad2Deg :: Float -> Float
rad2Deg x = 180 * (x / pi)

deg2Rad :: Float -> Float
deg2Rad x = (x / 180) * pi

-- Get the angle between two unit vectors, in degrees
getAngle :: V.Vector Double -> V.Vector Double -> Float
getAngle x y = rad2Deg . acos . realToFrac $ L.dot x y


vectorAtAngle :: Float -> String -> Picture
vectorAtAngle rot lab =
    pictures
      [ rotate rot (vectorP vlen)
      , translate x y $ scale 0.1 0.1 label
      ]
  where
    vlen = 180
    x = realToFrac $ 185 * sin (deg2Rad rot)
    y = realToFrac $ 185 * cos (deg2Rad rot)
    label = text $ "|" ++ lab ++ ">"

graphAxes :: Picture
graphAxes =
  color (greyN 0.8) $
    pictures
      [ line [(-3000, 0), (3000, 0)]
      , line [(0, -3000), (0, 3000)]
      ]

vectorP :: Float -> Picture
vectorP len =
  pictures
    [ line [(0,0), (0,len)]
    , line [(-3,len-5), (0,len)]
    , line [(0,len), (3,len-5)]
    ]
