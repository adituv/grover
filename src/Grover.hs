{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Grover where

import Data.Vector(Vector)
import qualified Data.Vector as Vector
import Linear

data GroverState = GroverState
  { target :: Vector Double
  , psi :: Vector Double
  , current :: Vector Double
  } deriving Show

defaultTarget :: Int -> Vector Double
defaultTarget n = Vector.cons 1
                $ Vector.replicate (2^n-1) 0

initState :: Vector Double -> GroverState
initState target = GroverState
    { target = target
    , psi = psi
    , current = psi
    }
  where
    extHadamard :: Int -> Vector Double
    extHadamard n = (\x -> x / sqrt (fromIntegral n))
                 <$> Vector.replicate n 1.0

    psi :: Vector Double
    psi = extHadamard (Vector.length target)

nextState :: GroverState -> GroverState
nextState state@GroverState{..} =
    state
      { current = reflectPsi (reflectTarget current) }
  where
    n = Vector.length psi
    reflectPsi x = (2 *!! outer psi psi !-! identity' n) !* x
    reflectTarget x = (identity' n !-! (2 *!! outer target target)) !* x

-- For some reason, Linear.identity doesn't play nicely with this...
identity' :: Int -> Vector (Vector Double)
identity' n = scaled $ Vector.replicate n 1
