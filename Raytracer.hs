{-# LANGUAGE TypeOperators #-}
module Raytracer (
    module World
    ,createCamera
    ,trace2Array
    ,trace2BMP
    ,Camera
) where
import Control.Monad.State
import Control.Applicative
import System.IO.Unsafe
import qualified Data.Array.Repa                  as R
import qualified Data.Array.Repa.Eval             as R
import qualified Data.Array.Repa.Unsafe           as R
import Data.Array.Repa.IO.BMP
import System.Random
import Data.Word
import Data.Time

import Ray
import World
import Vector


data Camera = Camera {
                cdir :: DoubleVector
                , cpoint :: DoubleVector
                , cup:: DoubleVector
                , fov:: Double
                }

                
createCamera:: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double) -> Double -> Camera                
createCamera (cdir1,cdir2,cdir3) (cpoint1,cpoint2,cpoint3) (cup1,cup2,cup3) fovIn = Camera { 
                                                                                cdir =  R.fromListUnboxed (R.ix1 3) [cdir1,cdir2,cdir3] 
                                                                                ,cpoint = R.fromListUnboxed (R.ix1 3) [cpoint1,cpoint2,cpoint3]
                                                                                ,cup = R.fromListUnboxed (R.ix1 3) [cup1,cup2,cup3]
                                                                                , fov = (fovIn*pi)/180
                                                                               }
                
                

cameraRay :: Camera->(Int, Int) -> Int -> Int -> Ray
cameraRay r@Camera{cdir = dir, cpoint = pnt, cup =up, fov= fov'} (maxX',maxY') x y =
     Ray{dir= normalize( R.computeUnboxedS(R.zipWith (+) pnt imagePoint)), point = pnt}  
            where u = crossProd dir up
                  v = crossProd u dir
                  halfWidth = 0.5--200
                  halfHeight = 1.0 --200
                  viewPlaneHalfWith = tan fov' --(pi/4.0)
                  aspectRatio = (fromIntegral maxX')/(fromIntegral maxY')
                  viewPlaneHalfHeight = viewPlaneHalfWith * aspectRatio
                  viewPlaneBottomLeftPoint = R.zipWith (-) ( R.zipWith (-) (dir) (R.map ( viewPlaneHalfHeight* ) v))  (R.map ( viewPlaneHalfWith* ) u)
                  yIncVector = R.map (*(2*viewPlaneHalfHeight/ (fromIntegral maxX'))) v
                  xIncVector = R.map (*(2*viewPlaneHalfWith/ (fromIntegral maxY'))) u
                  imagePoint = R.zipWith (+) (R.zipWith (+) viewPlaneBottomLeftPoint (R.map ( (fromIntegral y)* ) xIncVector)) (R.map ( ( fromIntegral x)* ) yIncVector)
                  
   
trace2Array::[Entity]-> Camera ->(Int,Int)-> Int -> R.Array R.U R.DIM2 Color
trace2Array ent camera (widht,height) bounses = finalasDouble
    where
     ents = createWorld ent
     ents' = execState ents emptyWorld
     indexs = [(0,0,0)| x<- [0..(widht-1)], y <- [0..(height-1)] ]
     image = R.fromListUnboxed (R.ix2 widht height) indexs
     finalasDouble = R.computeUnboxedS $ R.traverse image id (\f  x -> multPixtraceFunc  f x widht height ents' camera bounses) 

    
trace2BMP::[Entity]-> Camera ->(Int,Int)-> Int -> String -> IO()
trace2BMP ent camera (widht,height) bounses fName  = do
    let image = trace2Array ent camera (widht,height) bounses
    let final = R.computeUnboxedS $ R.map convertColtoFCol image
    writeImageToBMP ("./"++fName) final

multPixtraceFunc::(R.DIM2 -> Color) -> R.DIM2 -> Int -> Int -> World -> Camera-> Int -> Color
multPixtraceFunc f (R.Z R.:. ax R.:. ay) widht height w c  bounses=  foldr1 (avrageCol) [traceFunc f (R.Z R.:. ax R.:. ay) widht height w c bounses | _<-[1..5]]

avrageCol::Color -> Color-> Color
avrageCol (a,b,c) (a1,b1,c1) = ((a+a1)/2.0,(b+b1)/2.0,(c+c1)/2.0)

traceFunc :: (R.DIM2 -> Color) -> R.DIM2 -> Int -> Int -> World -> Camera -> Int-> Color --(Word8
traceFunc _ (R.Z R.:. ax R.:. ay) widht height w c bounses = unsafePerformIO(trace w (cameraRay (c) (widht,height) ax ay) 0 bounses)


trace :: World -> Ray -> Depth -> Int-> IO Color
trace w r@Ray{dir = dir', point = pnt} d bounses = do
    case d==bounses of
        True ->do
            return $ (0,0,0)
        False -> do 
            test <-intersectWorld r w 
            case test of
                Nothing -> return $ (0,0,0)
                (Just (obj@Object{shininess=shin},hitp)) -> do
                    let emittance = color obj
                    let norm = calcNormal obj hitp
                    lightIntens <- intersectLights pnt hitp norm w shin
                    newDir <- getSampledBiased norm 1
                    cos_theta' <- dotProd (normalize newDir) norm
                    let cos_theta =abs cos_theta' 
                    let brdf' = 2 * (reflectance obj) 
                    let brdf = minimum [brdf', 1/pi]
                    reflCol <- trace w (Ray{dir=newDir, point = hitp}) (d+1) bounses 
                    calcFinalCol emittance reflCol brdf lightIntens
                    
                    
                    
                 
            
calcFinalCol :: Color -> Color -> Double ->(Double,Color) -> IO Color
calcFinalCol (er,eg,eb) (rr,rg,rb) brf (lightInten,(lc1,lc2,lc3)) = do
    --putStrLn $ (show rb)++ "   " ++ (show eb) ++ "    " ++ show (round ((fromIntegral eb) * (light*0.8)))
    let res =(calc er rr lightInten lc1 brf,calc eg rg lightInten lc2 brf,calc eb rb lightInten lc3 brf)
    return res
    where calc a b l lc brd = clamp (a*0.1 +l*(a*0.9+b*brd+lc*l)) 0 255
           
          
    
getSampledBiased :: DoubleVector -> Double  -> IO DoubleVector -- -> StdGen
getSampledBiased dir pow  = do
    let dir' = normalize dir
    let o1 = normalize $ ortho dir
    let o2 = normalize $ crossProd dir o1
    randG <- newStdGen
    let (rand',_) = randomR (0,1000) randG 
    let rand = (rand'/1000)
    randG2 <- newStdGen
    let (rand2',_) = randomR (0,1000) randG2
    let rand2 = (rand2'/1000)
    let randV =  R.fromListUnboxed (R.ix1 2) [rand,rand2]
    let randV2 = R.fromListUnboxed (R.ix1 2) [(randV R.! (R.Z R.:. 0))*2.0*pi, (randV R.! (R.Z R.:. 1))**(1.0/(pow+1.0))]
    let onemius = sqrt (1.0 - ((randV2 R.! (R.Z R.:. 1))*(randV2 R.! (R.Z R.:. 1))))
    return $ (R.computeUnboxedS $ R.zipWith (+) ( R.zipWith (+) (R.map ((cos (randV2 R.! (R.Z R.:. 0)) * onemius)*) o1)
        (R.map ((sin (randV2 R.! (R.Z R.:. 0)) * onemius)*) o2)) (R.map (randV2 R.! (R.Z R.:. 0)*) dir))