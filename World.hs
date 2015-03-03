{-# LANGUAGE TypeOperators #-}
module World (
World, Object(..), Sphere (..),sphereNormal

) where
import qualified Data.Array.Repa as R
import Vector


type World = [Object]

data Object = Object {
                    shape :: Sphere
                    ,color :: DoubleVector
                    ,reflectance :: Double
                    }


data Sphere = Sphere {
        position :: DoubleVector
        ,radius :: Double
        }
    deriving (Show)

sphereNormal :: Sphere -> DoubleVector -> DoubleVector
sphereNormal s@Sphere{position= pos} pnt = normalize $ R.computeUnboxedS $ R.zipWith (-) pos pnt



