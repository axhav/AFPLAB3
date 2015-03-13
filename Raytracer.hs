{-# LANGUAGE TypeOperators #-}
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

dummyCam :: Camera
dummyCam = Camera { 
                cdir =  R.fromListUnboxed (R.ix1 3) [1,0,0] 
                ,cpoint = R.fromListUnboxed (R.ix1 3) [0,0,0]
                ,cup = R.fromListUnboxed (R.ix1 3) [0,1,0]
                , fov = pi/4
               } 
 
dummyRay :: Ray
dummyRay = Ray { dir =  R.fromListUnboxed (R.ix1 3) [5,0,0] 
                ,point = R.fromListUnboxed (R.ix1 3) [0,0,0]
               }
               
dummySphere :: Shape
dummySphere = Sphere {
                spos =  R.fromListUnboxed (R.ix1 3) [10,0,0] 
                ,radius = 2.0
            }
            
dummySphere2 :: Shape
dummySphere2 = Sphere {
                spos =  R.fromListUnboxed (R.ix1 3) [5,-3,-3] 
                ,radius = 2.0
            }

dummyPlane :: Shape
dummyPlane = Plane {
                ppos =  R.fromListUnboxed (R.ix1 3) [0,-1,0] 
                ,pnormal = R.fromListUnboxed (R.ix1 3) [0,1,0]
            }

dummyPlane2 :: Shape
dummyPlane2 = Plane {
                ppos =  R.fromListUnboxed (R.ix1 3) [0,0,1] 
                ,pnormal = R.fromListUnboxed (R.ix1 3) [0,0,1]
            }

dummyPlane3 :: Shape
dummyPlane3 = Plane {
                ppos =  R.fromListUnboxed (R.ix1 3) [4,0,0] 
                ,pnormal = R.fromListUnboxed (R.ix1 3) [1,0,0]
            }
            
dummyObj = Object{shape =dummySphere
             , color=(0,255,0) -- (R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 0}
           
            
dummyWorld :: World
dummyWorld = World{items = [Object{shape =dummySphere
             , color=(0,0,255) -- (R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 0},
             Object{shape =dummyPlane
             , color= (150,0,0) --(R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 1000}]
             ,lights = [Light{ 
                lpos =  R.fromListUnboxed (R.ix1 3) [0,10,0]
                ,lcolor = (255,255,255)
             }]
             } {-,
            
             Object{shape =dummyPlane2
             , color=(0,150,0) -- (R.fromListUnboxed (R.ix1 4) [0,0,0,0]) 
             ,reflectance = 1000},
             ] 
             -}

dummyWorld2 :: World 
dummyWorld2 = addLightToWorld (createLight (0.0,10.0,0.0) (t2c White))
    (addObjectToWorld (createSphere 2.0 (10.0,0.0,0.0) (t2c Blue) 0 )
     (addObjectToWorld (createPlane vDown vUp (150.0,0.0,0.0) 1000) emptyWorld ))
    

dummyVec1 :: DoubleVector
dummyVec1 = R.fromListUnboxed (R.ix1 3) [1,0,0]


dummyVec2 :: DoubleVector
dummyVec2 = R.fromListUnboxed (R.ix1 4) [20,1,0,0]

cameraRay :: Camera->(Int, Int) ->Int -> Int -> Ray
cameraRay r@Camera{cdir = dir, cpoint = pnt, cup =u} (maxX',maxY') x y =
     Ray{dir= ( R.computeUnboxedS( R.zipWith (-) imagePoint pnt)), point = pnt}
        where cam_right = crossProd dir u
              maxX = (fromIntegral maxX' )
              maxY = (fromIntegral maxY' )
              normX = ((fromIntegral x ) /maxX) -0.5
              normY =  ((fromIntegral y ) /maxY) -0.5
              imagePoint = (R.zipWith (+) (R.zipWith (+) (R.zipWith (+) (R.map (normX*) u)
                    (R.map (normY*) cam_right)) pnt) dir )

cameraRay2 :: Camera->(Int, Int) -> Int -> Int -> Ray
cameraRay2 r@Camera{cdir = dir, cpoint = pnt, cup =up, fov= fov'} (maxX',maxY') x y =
     Ray{dir= normalize( R.computeUnboxedS(R.zipWith (+) pnt imagePoint)), point = pnt}  
            where u = crossProd dir up
                  v = crossProd u dir
                  halfWidth = 1.0--200
                  halfHeight = 1.0 --200
                  viewPlaneHalfWith = tan fov' --(pi/4.0)
                  aspectRatio = 1
                  viewPlaneHalfHeight = viewPlaneHalfWith * aspectRatio
                  viewPlaneBottomLeftPoint = R.zipWith (-) ( R.zipWith (-) (dir) (R.map ( viewPlaneHalfHeight* ) v))  (R.map ( viewPlaneHalfWith* ) u)
                  xIncVector = R.map (*(2*halfWidth/ (fromIntegral maxX'))) v
                  yIncVector = R.map (*(2*halfHeight/ (fromIntegral maxY'))) u
                  imagePoint = R.zipWith (+) (R.zipWith (+) viewPlaneBottomLeftPoint (R.map ( (fromIntegral x)* ) xIncVector)) (R.map ( ( fromIntegral y)* ) yIncVector)

cameraRay3 :: Camera -> (Int, Int) -> Int -> Int -> Ray
cameraRay3 r@Camera{cdir = dir, cpoint = pnt, cup =up} (maxX,maxY) x y =
     Ray{dir= (R.computeUnboxedS pixelDir), point = pnt}  
            where cr = crossProd dir up
                  cu = crossProd cr dir
                  dist =(0.5 / (tan (90.0/2.0)))
                  pixelDir = (R.zipWith (+) (R.zipWith (+) (R.map (dist*) dir) (R.map ((0.5 - (fromIntegral y)/( fromIntegral(maxY -1)))*) cr) ) (R.map ((0.5 - (fromIntegral x)/( fromIntegral(maxX -1)))*) cu))
                  
main :: IO ()
main = do
    let path = "test.bmp"
    let widht = 200
    let height = 200
    putStrLn $ "Starting trace on a " ++ show widht ++ "x" ++ show height ++ " ..."
    let w = dummyWorld 
    let c = dummyCam
    t0 <- getCurrentTime
    let indexs = [(0,0,0)| x<- [0..(widht-1)], y <- [0..(height-1)] ]
    let image = R.fromListUnboxed (R.ix2 widht height) indexs
    let finalasDouble = R.computeUnboxedS $ R.traverse image id (\f x -> traceFunc f x widht height w c ) 
    let final = R.computeUnboxedS $ R.map convertColtoFCol finalasDouble
    
    writeImageToBMP ("./"++path) final
    t1 <- getCurrentTime
    
    putStrLn $ "Trace is done (in "++ show (diffUTCTime t1 t0) ++") creating image named " ++ show path
    {-
    ls <- sequence [trace w (cameraRay c (widht,height) x y) 0 | x <- [0..(widht-1)], y <- [0..(height-1)]]
    putStrLn $ "Trace is done creating image named " ++ show path
    let image = R.fromListUnboxed (R.ix2 widht height) ls
    writeImageToBMP ("./"++path) image
-}
--      
traceFunc :: (R.DIM2 -> Color) -> R.DIM2 -> Int -> Int -> World -> Camera -> Color --(Word8
traceFunc _ (R.Z R.:. ax R.:. ay) widht height w c = unsafePerformIO(trace w (cameraRay2 (c) (widht,height) ax ay) 0)


trace :: World -> Ray -> Depth -> IO Color
trace w r@Ray{dir = dir', point = pnt} d = do
    case d of
        2 ->do
            return $ (0,0,0)     -- byt 5an till dynamisk?
        _ -> do 
            test <-intersectWorld r w 
            case test of
                Nothing -> return $ (0,0,0)
                (Just (obj,hitp)) -> do
                    rand <- getStdGen
                    let emittance = color obj
                    let norm = calcNormal obj hitp
                    shadow <- intersectLights hitp w
                    newDir <- getSampledBiased norm 0 rand ---randVec norm 3.14 rand 
                    cos_theta <- dotProd (normalize newDir) norm
                    let brdf = 2 * (reflectance obj) * cos_theta
                    --putStrLn $ show newDir
                    reflCol <- trace w (Ray{dir=newDir, point = hitp}) (d+1) 
                    -- phong shading
                    --phongShader Ray{dir= dir', point = hitp} w (color obj) norm shadow
                    
                    -- end phong
                    --putStrLn $ show reflCol ++ "   " ++ show d  ++ "   " ++ show (cos_theta *180/pi)             
                    temp <- calcFinalCol emittance reflCol brdf shadow
                    --putStrLn $ "0: "++ show d
                    --putStrLn $ "1: "++ show temp
                    --putStrLn $ "2: "++ show reflCol
                    --putStrLn $ "3: "++ show brdf
                    
                    return temp
                    
                 
            
calcFinalCol :: Color -> Color -> Double ->Double -> IO Color
calcFinalCol (er,eg,eb) (rr,rg,rb) brf light = do
    --putStrLn $ (show rb)++ "   " ++ (show eb) ++ "    " ++ show (round ((fromIntegral eb) * (light*0.8)))
    let res =(calc er rr light,calc eg rg light,calc eb rb light)
    --putStrLn $ "1: " ++ show (round ((fromIntegral eb)*0.1)) 
    --putStrLn $ "2: " ++ show (round ((fromIntegral eb) * (light)*0.8))
    --putStrLn $ "3: " ++ show rb
    --putStrLn $ show $ ((brf * fromIntegral rb)+ ((fromIntegral eb) * (light)) + ((fromIntegral eb)*0.1))
    --putStrLn $ show res
    return res  --R.computeUnboxedS$ R.map round $ R.map (b*) $ R.map fromIntegral rc
    where calc a b l =  (brf *b) + (a*l*0.8) + (a*0.1) --(round ((brf * fromIntegral b)+ ((fromIntegral a) * (l*0.8)) + ((fromIntegral a)*0.1)))
    
getSampledBiased :: DoubleVector -> Double -> StdGen -> IO DoubleVector
getSampledBiased dir pow randG = do
    let dir' = normalize dir
    let o1 = normalize $ ortho dir
    let o2 = normalize $ crossProd dir o1
    let (rand',randG2) = randomR (-1000,1000) randG -- <- undefined
    let rand = (rand'/1000)
    let (rand2',randG3) = randomR (-1000,1000) randG2 -- <- undefined
    let rand2 = (rand2'/1000)
    let randV =  R.fromListUnboxed (R.ix1 2) [rand,rand2]
    let randV2 = R.fromListUnboxed (R.ix1 2) [(randV R.! (R.Z R.:. 0))*2.0*pi, (randV R.! (R.Z R.:. 0))**(1.0/(pow+1.0))]
    let onemius = sqrt (1.0 - ((randV2 R.! (R.Z R.:. 1))*(randV2 R.! (R.Z R.:. 1))))
    setStdGen randG3
    return $ (R.computeUnboxedS $ R.zipWith (+) ( R.zipWith (+) (R.map ((cos (randV2 R.! (R.Z R.:. 0)) * onemius)*) o1)
        (R.map ((sin (randV2 R.! (R.Z R.:. 0)) * onemius)*) o2)) (R.map (randV2 R.! (R.Z R.:. 0)*) dir))
    
    
