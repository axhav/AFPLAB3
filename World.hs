{-# LANGUAGE TypeOperators #-}
module World (
World(..),FinalColor,Color, Object(..), Shape (..),calcNormal,
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
,createObj
,Entity (..)
,createWorld
,v2Light
,v2Plaine
,v2Sphere
,clamp
) where
import qualified Data.Array.Repa as R
import Vector
import Data.Word
import Control.Monad.State.Lazy

-- | Color type 
type Color = (Double,Double,Double)
-- | Color type that is compatible with the Repa-IO functions
type FinalColor = (Word8,Word8,Word8)

-- | Textual interface for some standard colors 
data TextColor = Red | Blue | Green | Black | White

-- | Datatype managing the world
data World = World {
    items :: [Object]
    ,lights :: [Light]
    }

-- | A colective type for entitys that can be added into the world 
data Entity =  EntO Object | EntL Light

-- | The world state monad
type WorldWrapper = State World Entity  

-- | Datatype displaying visible objects  
data Object = Object {
                    shape :: Shape
                    ,color :: Color
                    ,reflectance :: Double
                    ,shininess::Double
                    }
    deriving (Show)

-- | datatype for the shapes of objects that can be displayed
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
    
-- | Function for calculating the normal at a specific point
calcNormal :: Object -> DoubleVector -> DoubleVector
calcNormal o@Object{shape =s@Sphere{spos = pos}} pnt = sphereNormal s pnt
calcNormal o@Object{shape =s@Plane{pnormal = norm}} _ = norm 

-- | Heplerfunction for calculating the shape of a sphere
sphereNormal :: Shape -> DoubleVector -> DoubleVector
sphereNormal s@Sphere{spos= pos} pnt = normalize $ 
                                    R.computeUnboxedS $ R.zipWith (-)  pnt pos

                                    
                                    
-- | Color management functions

-- | Color addition function 
cadd :: Color -> Color -> Color
cadd (r1,g1,b1) (r2,g2,b2) = (r1 + r2,g1 + g2,b1 + b2)

-- | Color multiplication function
cmul :: Color -> Double -> Color
cmul (r,g,b) d = (r*d,g*d,b*d)

-- | Helper function to convert a color of type Double to Word8
colRound :: Double -> Word8
colRound d | d >= 255.0 = 255
           | d <= 0.0 = 0
           | otherwise = round d
           
-- | Function to convert a color of type Double to Word8
convertColtoFCol :: Color -> FinalColor 
convertColtoFCol (r,g,b) = (colRound r, colRound g, colRound b)

-- | Function to convert the text color interface to an actual color
t2c :: TextColor -> Color
t2c Red = (255.0,0.0,0.0)
t2c Green = (0.0,255.0,0.0)
t2c Blue = (0.0,0.0,255.0)
t2c Black = (0.0,0.0,0.0)
t2c White = (255.0,255.0,255.0)

--

-- |Constructor Functions

-- Standard Array functions
-- | A Up Vector for the simple constructors
vUp :: (Double,Double,Double)
vUp = (0.0,1.0,0.0)

-- | A Down Vector for the simple constructors
vDown :: (Double,Double,Double)
vDown = (0.0,-1.0,0.0)

-- | A Forward Vector for the simple constructors
vForward :: (Double,Double,Double)
vForward = (1.0,0.0,0.0)

-- | A Backward Vector for the simple constructors
vBackward :: (Double,Double,Double)
vBackward = (-1.0,0.0,0.0)

-- | A Right Vector for the simple constructors
vRight:: (Double,Double,Double)
vRight = (1.0,0.0,1.0)

-- | A Left Vector for the simple constructors
vLeft :: (Double,Double,Double)
vLeft = (0.0,0.0,-1.0)


-- | World Construction

-- | Constrictor function to create an empty world 
emptyWorld :: World
emptyWorld = World {
    items = []
    ,lights = []
    }

-- | Function to create a world from a list of entitys
createWorld::[Entity] -> State World ()
createWorld (e:[]) = createObj e
createWorld (e:el) =  do
    createObj e
    createWorld el
     

-- | Function to add an object to the world state monad
createObj :: Entity -> State World ()
createObj (EntL e) = modify (addLightToWorld e)
createObj (EntO e) = modify (addObjectToWorld e)

-- | Function to add an object to an existing world
addObjectToWorld :: Object -> World -> World
addObjectToWorld o w@World{items= i} = w{items= (i ++ [o]) }

-- | Function to add a light to an existing world
addLightToWorld :: Light -> World -> World
addLightToWorld l w@World{lights= ls} = w{lights= (ls ++ [l]) }

-- | Constructor to create a sphere using the simpler type of vectors 
createSphere :: Double -> (Double, Double , Double) -> Color -> Double -> Double-> Object
createSphere rad (x,y,z) col ref shin = Object{ shape=Sphere{
    spos =  R.fromListUnboxed (R.ix1 3) [x,y,z]
    ,radius = rad}
    ,color = col
    ,shininess = shin
    ,reflectance = (clamp ref 0.0 1.0)
    }

-- | Constructor function to go from Repa array to an Sphere 
v2Sphere::DoubleVector ->Color ->Double -> Double -> Double-> Object
v2Sphere pos colorIn rad ref shin = Object{ shape=Sphere{
    spos =  pos
    ,radius = rad}
    ,color = colorIn
    , reflectance = (clamp ref 0.0 1.0)
    ,shininess = shin
    }

-- | Constructor function to create a plane using the simpler types of vectors 
createPlane ::(Double, Double , Double) ->(Double, Double , Double)
                                           -> Color -> Double ->Double -> Object
createPlane (x,y,z) (nx,ny,nz) colIn ref shin =(v2Plaine 
            (R.fromListUnboxed (R.ix1 3) [x,y,z]) ( R.fromListUnboxed (R.ix1 3)
            [nx,ny,nz]) colIn ref shin)

-- | Constructor function to go from Repa array to an Plane 
v2Plaine::DoubleVector ->DoubleVector ->Color -> Double->Double -> Object
v2Plaine pposIn pnormalIn colorIn refIn shin = Object{ shape=Plane{
                            ppos =  pposIn
                            ,pnormal = pnormalIn}
                            ,color = colorIn
                            , reflectance = clamp refIn 0.0 1.0
                            ,shininess = shin
                            }
                            
-- | Constructor function to create a light using the simpler types of vectors 
createLight ::(Double, Double , Double) -> Color -> Light
createLight (x,y,z) (col1,col2,col3) = (v2Light
                        (R.fromListUnboxed (R.ix1 3)
                        [x,y,z]) (R.fromListUnboxed (R.ix1 3)[col1,col2,col3]))
          
-- | Constructor function to go from Repa array to an light           
v2Light::DoubleVector -> DoubleVector -> Light
v2Light pos colorIn =  Light{
    lpos = pos
    ,lcolor = (colorIn R.! (R.Z R.:. 0),
                colorIn R.! (R.Z R.:. 1),colorIn R.! (R.Z R.:. 2))
    }
    
-- | Hepler function clamp ported from GLSL
clamp:: Double -> Double -> Double -> Double
clamp a min max | a < min = min
                | a > max = max
                | otherwise = a