module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import System.Random
import System.Environment

data Star = Star 
    { x :: Float
    , y :: Float
    , z :: Float
    } deriving (Show)

data Starfield = Starfield [Star]

newtype MaxSpeed = MaxSpeed Float

width, height :: Float 
width = 1024
height = 800

window :: Display
window = InWindow "Starfield" (1024, 800) (10, 10)

background :: Color
background = black

renderStar :: Star -> IO Picture
renderStar star = pure $ translate (x star) (y star) $ color white $ circleSolid 1

createStar :: MaxSpeed -> IO Star
createStar (MaxSpeed maxSpeed) = do
    x <- randomRIO ((-width / 2), width / 2)
    y <- randomRIO ((-height / 2), height / 2)
    z <- randomRIO (1, maxSpeed)
    pure $ Star x y z

render :: Starfield -> IO Picture
render (Starfield a) = do
    stars <- mapM renderStar a
    pure $ pictures stars

offScreen :: Star -> Bool
offScreen star
  | x star > (width / 2) - 10 = True
  | y star > height / 2 = True
  | x star < (-(width / 2)) = True
  | y star < (-(height / 2)) = True
  | otherwise = False

moveStar :: Float -> Star -> IO Star
moveStar time star = 
    if offScreen star
        then (createStar (MaxSpeed 20)) 
        else pure $ Star (x star + xrate) (y star + yrate) (z star) 
            where
                xrate = ((x star) - centerX) * (z star) * time / 10 -- ^ The further from the centre the faster it gets
                yrate = ((y star) - centerY) * (z star) * time / 10 -- ^ Ditto
                centerX = 0
                centerY = 0

moveStars :: Float -> Starfield -> IO Starfield
moveStars time (Starfield a) = do
    newStars <- mapM (moveStar time) $ a
    pure $ Starfield newStars

initialState :: Int -> MaxSpeed -> IO Starfield
initialState numStars maxSpeed = do
    stars <- sequence $ take numStars $ repeat $ createStar maxSpeed
    pure $ Starfield stars

--f :: Controller -> IO ()
f = const $ return ()

main :: IO ()
main = do
    args <- getArgs
    let numStars = 5000
        maxSpeed = MaxSpeed $ read (args !! 0)
    stars <- initialState numStars maxSpeed
    animateIO window background (frame stars) f
        where
            frame :: Starfield -> Float -> IO Picture
            frame stars seconds = do
                stars <- moveStars seconds stars
                render stars

