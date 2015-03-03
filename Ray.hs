{-# LANGUAGE TypeOperators #-}
module Ray (
Depth, Color , Ray (..), intersectB,intersectWorld,intersectP
) where

import World
import Vector
import qualified Data.Array.Repa as R
import Control.Monad.State


type Depth = Int
type Color = DoubleVector

type Arr = R.Array R.U (R.Z R.:. Int R.:. Int) Int 



data Ray = Ray {
                dir :: DoubleVector
                ,point :: DoubleVector
                }


-- | Should back the first object that was intersected and the intersection point
intersectWorld :: Ray -> World -> IO (Maybe (Object, DoubleVector))
intersectWorld ray@Ray{point= o} w = do
    objs <- filterM (\x -> intersectB ray x) w
    case objs of
        [] -> return Nothing
        _ -> do
            intp <- mapM (intersectP ray) objs
            let index = findShortest o intp
            return $ Just (objs !! index , intp !! index) 
            
intersectP :: Ray -> Object -> IO DoubleVector
intersectP ray@Ray{dir=d , point=o} obj@Object{shape=s@Sphere{position=c, radius = r}} = do
    loc <- (dotProd d $ R.computeUnboxedS $ R.zipWith (-) o c)
    let p = - loc
    let q1 = sqrt ((loc*loc) - ((dist o c)*(dist o c)) + (r*r))
    let q2 = -(sqrt $ (loc*loc) - ((dist o c)*(dist o c)) + (r*r))
    case (p + q1) < (p + q2) of
        True  -> return $ R.computeUnboxedS $ R.map ((p+q1)*) d
        False -> return $ R.computeUnboxedS $ R.map ((p-q1)*) d
    

intersectB :: Ray -> Object -> IO Bool
intersectB ray@Ray{dir=d , point=o} obj@Object{shape = s@Sphere{position=c, radius = r}} = do 
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

