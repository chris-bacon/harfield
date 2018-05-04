module Main where

import Graphics.Gloss
import System.Random
import System.Environment

data Star = Star 
    { x :: Float
    , y :: Float
    , z :: Float
    } deriving (Show)

data Starfield = Starfield [Star]

width, height :: Float 
width = 1024
height = 800

window :: Display
window = InWindow "Starfield" (1024, 800) (10, 10)

background :: Color
background = black

renderStar :: Star -> Picture
renderStar star = translate (x star) (y star) $ color white $ circleSolid 1

createStar :: Float -> IO Star
createStar maxSpeed = do
    x <- randomRIO ((-width / 2), width / 2)
    y <- randomRIO ((-height / 2), height / 2)
    z <- randomRIO (1, maxSpeed)
    pure $ Star x y z

render :: Starfield -> Picture
render (Starfield a) = pictures $ fmap renderStar a

moveStar :: Float -> Star -> Star
moveStar time star = Star (x star + xrate) (y star + yrate) (z star)
    where
        xrate = ((x star) - centerX) * (z star) * time / 10 -- ^ The further from the centre the faster it gets
        yrate = ((y star) - centerY) * (z star) * time / 10 -- ^ Ditto
        centerX = 0
        centerY = 0

moveStars :: Float -> Starfield -> Starfield
moveStars time (Starfield a) = Starfield $ fmap (moveStar time) a

initialState :: Int -> Float -> IO Starfield
initialState numStars maxSpeed = do
    stars <- sequence $ take numStars $ repeat $ createStar maxSpeed
    pure $ Starfield stars

main :: IO ()
main = do
    args <- getArgs
    let numStars = 5000
        maxSpeed = read (args !! 0) :: Float
    stars <- initialState numStars maxSpeed
    animate window background (frame stars)
        where
            frame :: Starfield -> Float -> Picture
            frame stars seconds = render $ moveStars seconds stars

