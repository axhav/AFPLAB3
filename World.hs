{-# LANGUAGE TypeOperators #-}
module World (
World(..),Color, Object(..), Shape (..),calcNormal,
Light(..),cmul,colRound,cadd,convertColtoFCol

-- Creation functions
-- Standard Array functions
,vUp 
,vDown
,vForward
,vBackward 
,vRight
,vLeft
-- Color conviniece functions

,t2c 
--

-- | World Construction
,emptyWorld 
,addObjectToWorld
,addLightToWorld 
,createSphere
,createPlane
,createLight
,TextColor (..)

) where
import qualified Data.Array.Repa as R
import Vector
import Data.Word

type Color = (Double,Double,Double)--R.Array R.U (R.Z R.:. Int) Word8
type FinalColor = (Word8,Word8,Word8)

data TextColor = Red | Blue | Green | Black | White



data World = World {
    items :: [Object]
    ,lights :: [Light]
    }
    

data Object = Object {
                    shape :: Shape
                    ,color :: Color
                    ,reflectance :: Double
                    }
    deriving (Show)

data Shape = 
        Sphere {
        spos :: DoubleVector
        ,radius :: Double
        } 
        | Plane {
        ppos :: DoubleVector
        ,pnormal :: DoubleVector
        }
    deriving (Show)

data Light = Light{
    lpos :: DoubleVector
    ,lcolor:: Color
    }
    
    
calcNormal :: Object -> DoubleVector -> DoubleVector
calcNormal o@Object{shape =s@Sphere{spos = pos}} pnt = sphereNormal s pnt
calcNormal o@Object{shape =s@Plane{pnormal = norm}} _ = norm 
    
sphereNormal :: Shape -> DoubleVector -> DoubleVector
sphereNormal s@Sphere{spos= pos} pnt = normalize $ R.computeUnboxedS $ R.zipWith (-)  pnt pos

cadd :: Color -> Color -> Color
cadd (r1,g1,b1) (r2,g2,b2) = (r1 + r2,g1 + g2,b1 + b2)

cmul :: Color -> Double -> Color
cmul (r,g,b) d = (r*d,g*d,b*d)

colRound :: Double -> Word8
colRound d | d >= 255.0 = 255
           | d <= 0.0 = 0
           | otherwise = round d
           
convertColtoFCol :: Color -> FinalColor 
convertColtoFCol (r,g,b) = (colRound r, colRound g, colRound b)


-- |Constructor Functions

-- Standard Array functions
vUp :: (Double,Double,Double)
vUp = (0.0,1.0,0.0)

vDown :: (Double,Double,Double)
vDown = (0.0,-1.0,0.0)

vForward :: (Double,Double,Double)
vForward = (1.0,0.0,0.0)

vBackward :: (Double,Double,Double)
vBackward = (-1.0,0.0,0.0)

vRight:: (Double,Double,Double)
vRight = (1.0,0.0,1.0)

vLeft :: (Double,Double,Double)
vLeft = (0.0,0.0,-1.0)

-- Color conviniece functions

t2c :: TextColor -> Color
t2c Red = (1.0,0.0,0.0)
t2c Green = (0.0,1.0,0.0)
t2c Blue = (0.0,0.0,1.0)
t2c Black = (0.0,0.0,0.0)
t2c White = (1.0,1.0,1.0)
--


-- | World Construction

emptyWorld :: World
emptyWorld = World {
    items = []
    ,lights = []
    }
    
addObjectToWorld :: Object -> World -> World
addObjectToWorld o w@World{items= i} = w{items= (i ++ [o]) }

addLightToWorld :: Light -> World -> World
addLightToWorld l w@World{lights= ls} = w{lights= (ls ++ [l]) }

createSphere :: Double -> (Double, Double , Double) -> Color -> Double -> Object
createSphere rad (x,y,z) col ref = Object{ shape=Sphere{
    spos =  R.fromListUnboxed (R.ix1 3) [x,y,z]
    ,radius = rad}
    ,color = col
    , reflectance = ref
    }

createPlane :: (Double, Double , Double) ->(Double, Double , Double) -> Color -> Double -> Object
createPlane (x,y,z) (nx,ny,nz) col ref = Object{ shape=Plane{
    ppos =  R.fromListUnboxed (R.ix1 3) [x,y,z]
    ,pnormal =  R.fromListUnboxed (R.ix1 3) [nx,ny,nz]}
    ,color = col
    , reflectance = ref
    }           
   
createLight ::(Double, Double , Double) -> Color -> Light
createLight (x,y,z) col = Light{
    lpos = R.fromListUnboxed (R.ix1 3) [x,y,z]
    ,lcolor = col
    }
    