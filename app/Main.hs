module Main where

import Graphics.Gloss
import System.Random

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
window = InWindow "My window" (1024, 800) (10, 10)

background :: Color
background = black

renderStar :: Star -> Picture
renderStar star = translate (x star) (y star) $ color white $ circleSolid 1

createStar :: IO Star
createStar = do
    x <- randomRIO ((-width/2), width/2)
    y <- randomRIO ((-height/2), height/2)
    z <- randomRIO (1, 3)
    pure $ Star x y z

render :: Starfield -> Picture
render (Starfield a) = pictures $ fmap renderStar a

moveStar :: Float -> Star -> Star
moveStar time star = Star (x star + xrate) (y star + yrate) (z star)
    where
        xrate = (x star) - centerX * (z star) * time
        yrate = (y star) - centerY * (z star) * time
        centerX = width / 2
        centerY = height / 2

moveStars :: Float -> Starfield -> Starfield
moveStars time (Starfield a) = Starfield $ fmap (moveStar time) a

initialState :: Int -> IO Starfield
initialState numStars = do
    stars <- sequence $ take numStars $ repeat $ createStar
    pure $ Starfield stars

main :: IO ()
main = do
    stars <- initialState 50
    animate window background (frame stars)
        where
            frame :: Starfield -> Float -> Picture
            frame stars seconds = render $ moveStars seconds stars

