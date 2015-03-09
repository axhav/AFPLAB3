{-# LANGUAGE TypeOperators #-}
module Ray (
Depth , Ray (..), intersectB,intersectWorld,intersectP
) where

import World
import Vector
import qualified Data.Array.Repa as R
import Control.Monad.State

type Depth = Int

data Ray = Ray {
                dir :: DoubleVector
                ,point :: DoubleVector
                }
    deriving (Show) 


-- | Should back the first object that was intersected and the intersection point
intersectWorld :: Ray -> World -> IO (Maybe (Object, DoubleVector))
intersectWorld ray@Ray{dir = d, point= o} w = do
    let r' = Ray{ dir = normalize d, point = o }
    objs <- filterM (\x -> intersectB r' x) w
    case objs of
        [] -> return Nothing
        _ -> do
            intp <- mapM (intersectP r') objs
            
            let index = findShortest o intp
            return $ Just (objs !! index , intp !! index) 
            
intersectP :: Ray -> Object -> IO DoubleVector
intersectP ray@Ray{dir=d , point=o} obj@Object{shape=s@Sphere{spos=c, radius = r}} = do
    let d' = normalize d
    loc <- (dotProd d' $ R.computeUnboxedS $ R.zipWith (-) o c)
    let p = - loc
    let q1 = sqrt ((loc*loc) - ((dist o c)*(dist o c)) + (r*r))
    let q2 = -(sqrt $ (loc*loc) - ((dist o c)*(dist o c)) + (r*r))
    case ((p + q1) > (p + q2)) of
        True  -> do
            --putStrLn $ show (p+q2)
            return $ R.computeUnboxedS $ R.map ((p+q2)*) d'
        False ->do
            --putStrLn $ show  (p+q1)
            return $ R.computeUnboxedS $ R.map ((p+q1)*) d'
intersectP ray@Ray{dir=d , point=o} obj@Object{shape=s@Plane{ppos=c, pnormal = n}} = do
    let d' = normalize d
    denum <- dotProd d' n  
    let sub = R.computeUnboxedS $ R.zipWith (-) c o
    l' <- dotProd sub n
    return $ R.computeUnboxedS $ R.map ((l'/denum)*) d'

intersectB :: Ray -> Object -> IO Bool
intersectB ray@Ray{dir=d , point=o} obj@Object{shape = s@Sphere{spos=c, radius = r}} = do 
    let sub' = R.computeUnboxedS $ R.zipWith (-) o c
    let d' = normalize d
    s1' <- (dotProd d' sub')
    let s1 = s1'*s1'
    let s2 = (vLength sub')*(vLength sub')
    case (s1-s2 + (r*r)) > 0 of
        False -> return False
        True -> return True
intersectB ray@Ray{dir=d , point=o} obj@Object{shape = s@Plane{ppos=c, pnormal = n}} = do 
    let d' = normalize d
    s1 <- (dotProd d' n)
    case s1 /= 0  of
        False -> return False
        True -> do
            let sub = R.computeUnboxedS $ R.zipWith (-) c o
            l' <- dotProd sub n
            case (l'/s1) > 0 of
                False -> return False
                True -> return True   
        {-
    fstcheck <- dotProd sub' d'
    putStrLn $ show fstcheck
    case fstcheck < 0 of
        False -> return False
        True -> do
            d1 <- dotProd sub' sub' 
            let sndcheck = d1 - (r*r)
            putStrLn $ show sndcheck
            case sndcheck > 0 of
                True -> return True
                False -> return False
-}