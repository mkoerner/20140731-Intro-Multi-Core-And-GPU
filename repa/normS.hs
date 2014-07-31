{-# LANGUAGE TypeOperators #-}
import Data.Array.Repa as R
import Data.Array.Repa.Index as I
import qualified Data.Array.Repa.Repr.Vector as V
import Criterion
import Criterion.Main (defaultMain)

-- Interaction -> 10000
shape :: DIM1
shape = ix1 10000

m :: Array U DIM1 Double
m = computeS $ fromFunction shape (\(Z :. i) -> fromIntegral i)

norm :: Array U DIM1 Double -> Double
norm m = R.sumAllS $ R.map (\x -> x * x) m

main = defaultMain [
           bench "map" $ nf (\i -> norm m) 0 
     ]
