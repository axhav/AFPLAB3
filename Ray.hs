{-# LANGUAGE TypeOperators #-}
-- | Module for rays and ray calculations
module Ray (
Depth 
, Ray (..)
, intersectB
, intersectWorld
, intersectP
, intersectLights
, phongShader
) where

import World
import Vector
import qualified Data.Array.Repa as R
import Control.Monad.State

-- | 
type Depth = Int

-- | Data type for creating rays 
data Ray = Ray {
                dir :: DoubleVector
                ,point :: DoubleVector
                }
    deriving (Show) 


-- | Should back the first object that was intersected and the intersection point
intersectWorld :: Ray -> World -> IO (Maybe (Object, DoubleVector))
intersectWorld ray@Ray{dir = d, point= o} w'@World{items = w} = do
    let r' = Ray{ dir = normalize d, point = o }
    objs <- filterM (\x -> intersectB r' x) w
    case objs of
        [] -> return Nothing
        _ -> do
            intp <- mapM (intersectP r') objs
            
            let index = findShortest o intp
            return $ Just (objs !! index , intp !! index) 
         
-- | Calculates Shadowrays returns a combinded color of the visible lights 
-- and an intensity factor of how much light the point is exposed to
intersectLights :: DoubleVector -> DoubleVector -> DoubleVector -> World 
                                                        -> IO(Double, Color) 
intersectLights cP hitp norm w@World{lights = []} = return (0,(0,0,0))
intersectLights cP hitp norm w@World{items = o, lights = (l:ls)} = do
    res <- intersectLight cP hitp norm w l

    res2 <-intersectLights cP hitp norm (World{items = o , lights = ls})
    let fres =(((fst res)+fst(res2)) /2.0,(addCol (snd res)(snd res2)))
    return $ fres

-- | convenience function to add two colors
addCol:: Color->Color -> Color
addCol (a,b,c) (a1,b1,c1)=(a+a1,b+b1,c+c1)    

-- | convenience function to multiply two colors
mulCol::Color -> Double -> Color
mulCol (a,b,c) m = (a*m,b*m,c*m)

-- Calculates the shadowray for one specific light and returns the color of the 
-- light and the intensity
intersectLight ::DoubleVector -> DoubleVector -> DoubleVector -> World -> Light
                                                            -> IO(Double, Color)
intersectLight 
    cPos hitp norm w@World{items = o} l@Light{lpos = pos, lcolor=lc}= do
    let directionToL = R.computeUnboxedS $ ( R.zipWith (-)  pos hitp )
    let cPos2htp       = R.computeUnboxedS $ ( R.zipWith (-) cPos hitp)
    let dir'= normalize directionToL
    obj <- intersectWorld Ray{point = hitp, dir = dir'} w
    case obj of 
        Nothing -> do
                   let halfDir' = fun directionToL  cPos2htp
                   specang1' <- dotProd halfDir' norm
                   let temp' = (maximum [specang1', 0])**5
                   return(temp',mulCol lc temp') 
        Just (obj,hitpoint) -> do
            let llenght = vLength directionToL
            let olenght = (vLength $ fun2 hitp hitpoint)
            case llenght > olenght of
                True -> return (0.0, (0,0,0))
                False -> do
                    let halfDir = fun directionToL cPos2htp     
                    specang1 <- dotProd halfDir norm
                    let temp = (maximum [specang1, 0])**5
                    return(temp,mulCol lc temp)
    where fun a b = (normalize $ R.computeUnboxedS $ R.zipWith (+) a b)
          fun2 a b = ( R.computeUnboxedS $ R.zipWith (-) a b)
         
-- | Intersection Tests 

-- | Intersection test between a ray and an object returns the hit point, 
-- expects that `intersectB` has been run before to not get complex solutions
intersectP :: Ray -> Object -> IO DoubleVector
intersectP 
    ray@Ray{dir=d , point=o} obj@Object{shape=s@Sphere{spos=c, radius = r}} = do
    let d' = normalize d
    loc <- (dotProd d' $ R.computeUnboxedS $ R.zipWith (-) o c)
    let p = - loc
    let q1 = sqrt ((loc*loc) - ((dist o c)*(dist o c)) + (r*r))
    let q2 = -(sqrt $ (loc*loc) - ((dist o c)*(dist o c)) + (r*r))
    case ((p + q1) > (p + q2)) of
        True  -> do
            return $ R.computeUnboxedS $ R.map ((p+q2)*) d'
        False ->do  
            return $ R.computeUnboxedS $ R.map ((p+q1)*) d'
intersectP 
    ray@Ray{dir=d , point=o} obj@Object{shape=s@Plane{ppos=c, pnormal = n}} = do
    let d' = normalize d
    denum <- dotProd d' n  
    let sub = R.computeUnboxedS $ R.zipWith (-) c o
    l' <- dotProd sub n
    return $ R.computeUnboxedS $ R.map ((l'/denum)*) d'

-- | IntersectB performs an intersection test, returns if the object is hit or 
-- not 
intersectB :: Ray -> Object -> IO Bool
intersectB 
    ray@Ray{dir=d , point=o} obj@Object{shape = s@Sphere{spos=c, radius = r}}=do 
    let sub' = R.computeUnboxedS $ R.zipWith (-) o c
    let d' = normalize d
    s1' <- (dotProd d' sub')
    case s1' < 0 of
        False -> return False
        True -> do
            d1 <- dotProd sub' sub' 
            let sndcheck = d1 - (r*r)
            case sndcheck > 0 of
                False -> return False
                True -> do    
                    let s1 = s1'*s1'
                    let s2 = (vLength sub')*(vLength sub')
                    case (s1-s2 + (r*r)) > 0 of
                        False -> return False
                        True -> return True
intersectB 
    ray@Ray{dir=d , point=o} obj@Object{shape = s@Plane{ppos=c, pnormal = n}}=do 
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

-- | Test function to run a "standard" phongshader gives a cartoonish result
-- with bad performence only used for testing and reference imagry
phongShader :: Ray -> World -> Color -> DoubleVector -> Double -> IO Color
phongShader ray@Ray{dir=d, point=p} 
    w@World{lights=(l@Light{lpos = lpos1, lcolor = col}:ls)} 
   (r,g,b) norm shadow = do
    let ambient = ( r*0.1,g*0.1,b*0.1)
    let specular = col
    let lightDir = R.computeUnboxedS $ R.zipWith (-) lpos1 p
    lamb1 <- dotProd lightDir norm
    let labertian = dmax lamb1 0.0
    case labertian > 0.0 of
        False -> return ambient
        True -> do
            let halfDir=normalize $ R.computeUnboxedS $ R.zipWith (-) lightDir d
            specang1 <- dotProd halfDir norm
            let specAngle = dmax specang1 0.0
            let specular = specAngle ** 16.0
            return $ (cadd (cmul(cadd (cmul (r,g,b) labertian)
                (cmul col specular)) shadow) ambient )  
                
-- | Convinience funtion ported from GLSL 
dmax :: Double -> Double -> Double 
dmax d1 d2 | d1 > d2 = d1
          | otherwise  = d2

-- | Convinience funtion ported from HLSL           
saturate :: Double -> Double 
saturate d | d >= 1.0 = 1.0
           | d <= 0.0 = 0.0
           | otherwise = d