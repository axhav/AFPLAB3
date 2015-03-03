{-# LANGUAGE TypeOperators #-}
module Vector where
import Data.List
import qualified Data.Array.Repa as R

type DoubleVector = R.Array R.U (R.Z R.:. Int) Double

vLength :: DoubleVector -> Double
vLength v = sqrt $ pow2 0 + pow2 1 + pow2 3
     where pow2 i = (v R.! (R.Z R.:. i) )*(v R.! (R.Z R.:. i) )

normalize :: DoubleVector -> DoubleVector
normalize v = R.computeUnboxedS $ R.map (/vLength v)  v 

-- | A DOT PRODUCT!!!!!11eleven        
dotProd ::DoubleVector -> DoubleVector -> IO Double
dotProd v1 v2 =  R.sumAllP $ R.zipWith (*) v1 v2

dist :: DoubleVector -> DoubleVector -> Double 
dist v1 v2 = sqrt $ (pow2 0) + (pow2 1) + (pow2 2) 
    where pow2 i = (((v1 R.! (R.Z R.:. i) ) - (v2 R.! (R.Z R.:. i)))*((v1 R.! (R.Z R.:. i)) - (v2 R.! (R.Z R.:. i))))

findShortest :: DoubleVector -> [DoubleVector] -> Int
findShortest p (vs) = snd $ head $ sortBy (sortGT p) (zip vs [0..])
    where sortGT pt x y | dist p (fst x) > dist p (fst y) = GT
                        | dist p (fst x) < dist p (fst y) = LT
                        | dist p (fst x) == dist p (fst y) = EQ
