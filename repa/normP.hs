{-# LANGUAGE TypeOperators #-}
import Data.Array.Repa as R
import Data.Array.Repa.Index as I
import Control.Monad.Identity
import Criterion
import Criterion.Main (defaultMain)

-- Interaction -> 10000
shape :: DIM1
shape = ix1 10000

m :: Array U DIM1 Double
m = computeS $ fromFunction shape (\(Z :. i) -> fromIntegral i)

norm :: Array U DIM1 Double -> Double
norm m = runIdentity $ R.sumAllP $ R.map (\x -> x * x) m

main = defaultMain [
           bench "map" $ nf (\i -> norm m) 0 
     ]
