{-# LANGUAGE TypeOperators #-}
module Ray (
Depth, Color , Ray (..), intersect
) where
import World
import qualified Data.Array.Repa as R

type Depth = Int
type Color = DoubleVector

type Arr = R.Array R.U (R.Z R.:. Int R.:. Int) Int 

type DoubleVector = R.Array R.U (R.Z R.:. Int) Double

data Ray = Ray {
                dir :: DoubleVector
                ,point :: DoubleVector
                }


-- | A DOT PRODUCT!!!!!11eleven        
dotProd ::DoubleVector -> DoubleVector -> IO Double
dotProd v1 v2 =  R.sumAllP $ R.zipWith (*) v1 v2

               
intersect :: Ray -> Object -> IO Bool
intersect ray@Ray{dir=d , point=o} obj@Sphere{position=c, radius = r} = do 
    let sub' = R.computeUnboxedS $ R.zipWith (-) o c
    fstcheck <- dotProd sub' d
    case fstcheck < 0 of
        False -> return False
        True -> do
            d1 <- dotProd sub' sub' 
            let sndcheck = d1 - (r*r)
            case sndcheck > 0 of
                True -> return True
                False -> return False

