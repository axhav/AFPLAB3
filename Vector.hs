{-# LANGUAGE TypeOperators #-}
-- | Module for common functions used in a vector environment
module Vector where
import Data.List
import qualified Data.Array.Repa as R

-- | Type for one dimensional arrays of type double
type DoubleVector = R.Array R.U (R.Z R.:. Int) Double
type Point = DoubleVector
-- | Function to calculate the length of a vector
vLength :: DoubleVector -> Double
vLength v = sqrt $ pow2 0 + pow2 1 + pow2 2
     where pow2 i = (v R.! (R.Z R.:. i) )*(v R.! (R.Z R.:. i) )

-- | Function to normalize a vector
normalize :: DoubleVector -> DoubleVector
normalize v = R.computeUnboxedS $ R.map (/vLength v)  v 

-- | Function to calculate the dot product of two vectors        
dotProd ::DoubleVector -> DoubleVector -> IO Double
dotProd v1 v2 =  R.sumAllP $ R.zipWith (*) v1 v2

-- | cross prod for 3 dim vectors
crossProd :: DoubleVector -> DoubleVector -> DoubleVector
crossProd v1 v2 = R.fromListUnboxed (R.ix1 3) 
                  [((ind v1 1) *(ind v2 2)) -((ind v1 2) *(ind v2 1)) 
                  ,((ind v1 2) *(ind v2 0)) -((ind v1 0) *(ind v2 2))
                  ,((ind v1 0) *(ind v2 1)) -((ind v1 1) *(ind v2 0))]
    where ind v i = (v R.! (R.Z R.:. i) )

-- | Orthogonal projection of a double vector    
ortho :: DoubleVector -> DoubleVector
ortho v = case (v R.! (R.Z R.:. 0)) >(v R.! (R.Z R.:. 2)) of
    True -> R.fromListUnboxed (R.ix1 3) [-(v R.! (R.Z R.:. 1)),v R.! (R.Z R.:. 0),0.0]
    False ->  R.fromListUnboxed (R.ix1 3) [0.0,-(v R.! (R.Z R.:. 2)),v R.! (R.Z R.:. 1)]

-- | Function to calculate the distance between two points represented as double
-- vectors
dist :: Point -> Point -> Double 
dist v1 v2 = sqrt $ (pow2 0) + (pow2 1) + (pow2 2) 
    where pow2 i = (((v1 R.! (R.Z R.:. i) ) - (v2 R.! (R.Z R.:. i)))*((v1 R.! (R.Z R.:. i)) - (v2 R.! (R.Z R.:. i))))

-- | Function to get the index of the shortest point in a list of points
-- calculated from one point
findShortest :: Point -> [Point] -> Int
findShortest p (vs) = snd $ head $ sortBy (sortGT p) (zip vs [0..])
    where sortGT pt x y | dist p (fst x) > dist p (fst y) = GT
                        | dist p (fst x) < dist p (fst y) = LT
                        | dist p (fst x) == dist p (fst y) = EQ
