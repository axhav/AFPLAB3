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
             ,reflectance = 1.0}]
             ,lights = [Light{ 
                lpos =  R.fromListUnboxed (R.ix1 3) [20,10,0]
                ,lcolor = (255,255,255)
             },Light{ 
                lpos =  R.fromListUnboxed (R.ix1 3) [5,7,0]
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
    let finalasDouble = R.computeUnboxedS $ R.traverse image id (\f  x ->  multPixtraceFunc  f x widht height w c ) 
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

multPixtraceFunc::(R.DIM2 -> Color) -> R.DIM2 -> Int -> Int -> World -> Camera -> Color
multPixtraceFunc f (R.Z R.:. ax R.:. ay) widht height w c =  foldr1 (avrageCol) [traceFunc f (R.Z R.:. ax R.:. ay) widht height w c | _<-[1..25]]

avrageCol::Color -> Color-> Color
--avrageCol (0,0,0) (a1,b1,c1) = (a1,b1,c1)
--avrageCol (a,b,c) (0,0,0) = (a,b,c)
avrageCol (a,b,c) (a1,b1,c1) = ((a+a1)/2.0,(b+b1)/2.0,(c+c1)/2.0)

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
                Nothing -> return $ (0,255,0)
                (Just (obj,hitp)) -> do
                    --rand <- getStdGen
                    let emittance = color obj
                    let norm = calcNormal obj hitp
                    lightIntens <- intersectLights pnt hitp norm w
                    newDir <- getSampledBiased norm 0 --rand ---randVec norm 3.14 rand
                    --putStrLn $ show newDir
                    cos_theta' <- dotProd (normalize newDir) norm
                    let cos_theta =abs cos_theta' 
                    let brdf' = 2 * (reflectance obj) * cos_theta
                    let brdf = minimum [brdf', 1/pi]
                    --putStrLn $ show newDir
                    reflCol <- trace w (Ray{dir=newDir, point = hitp}) (d+1) 
                    -- phong shading
                    --phongShader Ray{dir= dir', point = hitp} w (color obj) norm shadow
                    
                    -- end phong
                    --putStrLn $ show reflCol ++ "   " ++ show d  ++ "   " ++ show (cos_theta *180/pi)             
                    
                    --putStrLn $ "0: "++ show d
                    --putStrLn $ "1: "++ show temp
                    --putStrLn $ "2: "++ show reflCol
                    --putStrLn $ "3: "++ show brdf
                    calcFinalCol emittance reflCol brdf lightIntens
                    
                    
                    
                 
            
calcFinalCol :: Color -> Color -> Double ->(Double,Color) -> IO Color
calcFinalCol (er,eg,eb) (rr,rg,rb) brf (lightInten,(lc1,lc2,lc3)) = do
    --putStrLn $ (show rb)++ "   " ++ (show eb) ++ "    " ++ show (round ((fromIntegral eb) * (light*0.8)))
    let res =(calc er rr lightInten lc1 brf,calc eg rg lightInten lc2 brf,calc eb rb lightInten lc3 brf)
    --putStrLn $ show $ lightInten
    return res
    where calc a b l lc brd = a*0.1 +l*(a*0.9+b*brd+lc*l)
    
getSampledBiased :: DoubleVector -> Double  -> IO DoubleVector -- -> StdGen
getSampledBiased dir pow  = do
    let dir' = normalize dir
    let o1 = normalize $ ortho dir
    let o2 = normalize $ crossProd dir o1
    randG <- newStdGen
    let (rand',_) = randomR (-1000,1000) randG -- <- undefined
    let rand = (rand'/1000)
    randG2 <- newStdGen
    let (rand2',_) = randomR (-1000,1000) randG2 -- <- undefined
    let rand2 = (rand2'/1000)
    let randV =  R.fromListUnboxed (R.ix1 2) [rand,rand2]
    let randV2 = R.fromListUnboxed (R.ix1 2) [(randV R.! (R.Z R.:. 0))*2.0*pi, (randV R.! (R.Z R.:. 1))**(1.0/(pow+1.0))]
    let onemius = sqrt (1.0 - ((randV2 R.! (R.Z R.:. 1))*(randV2 R.! (R.Z R.:. 1))))
    --setStdGen randG3
    return $ (R.computeUnboxedS $ R.zipWith (+) ( R.zipWith (+) (R.map ((cos (randV2 R.! (R.Z R.:. 0)) * onemius)*) o1)
        (R.map ((sin (randV2 R.! (R.Z R.:. 0)) * onemius)*) o2)) (R.map (randV2 R.! (R.Z R.:. 0)*) dir))
    
    
