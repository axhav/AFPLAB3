{-# LANGUAGE TypeOperators #-}
module World (
World,Color, Object(..), Sphere (..),calcNormal

) where
import qualified Data.Array.Repa as R
import Vector
import Data.Word

type Color =  (Word8,Word8,Word8)--R.Array R.U (R.Z R.:. Int) Word8
type World = [Object]

data Object = Object {
                    shape :: Sphere
                    ,color :: Color
                    ,reflectance :: Double
                    }
    deriving (Show)


data Sphere = Sphere {
        position :: DoubleVector
        ,radius :: Double
        }
    deriving (Show)


calcNormal :: Object -> DoubleVector -> DoubleVector
calcNormal o@Object{shape =s@Sphere{position = pos}} pnt = sphereNormal s pnt
    
sphereNormal :: Sphere -> DoubleVector -> DoubleVector
sphereNormal s@Sphere{position= pos} pnt = normalize $ R.computeUnboxedS $ R.zipWith (-) pos pnt



