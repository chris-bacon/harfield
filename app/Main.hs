module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Simulate
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
window = InWindow "Starfield" (round width, round height) (10, 10)

background :: Color
background = black

renderStar :: Star -> Picture
renderStar star = translate (x star) (y star) $ color white $ circleSolid 1

createStar :: MaxSpeed -> IO Star
createStar (MaxSpeed maxSpeed) = do
    x <- randomRIO ((-width / 2), width / 2)
    y <- randomRIO ((-height / 2), height / 2)
    z <- randomRIO (1, maxSpeed)
    pure $ Star x y z

render :: Starfield -> Picture
render (Starfield a) = pictures $ renderStar <$> a

offScreen :: Star -> Float -> Bool
offScreen star time
  | x star * time >= (width / 2) = True
  | y star * time >= (height / 2) = True
  | x star * time <= (-(width / 2)) = True
  | y star * time <= (-(height / 2)) = True
  | otherwise = False

moveStar :: Float -> Star -> IO Star
moveStar time star = 
    if offScreen star time
        then createStar $ MaxSpeed 20
        else pure $ Star (x star + vx) (y star + vy) (z star) 
            where
                vx = ((x star) - center) * (z star) * time / 10 -- ^ The further from the centre the faster it gets
                vy = ((y star) - center) * (z star) * time / 10 -- ^ Ditto
                center = 0

moveStars :: Float -> Starfield -> IO Starfield
moveStars time (Starfield a) = do
    newStars <- mapM (moveStar time) $ a
    pure $ Starfield newStars

initialState :: Int -> MaxSpeed -> IO Starfield
initialState numStars maxSpeed = Starfield . take numStars . repeat <$> createStar maxSpeed

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--num", num] -> do
            let numStars = read (num) :: Int 
                fps      = 60
                maxSpeed = MaxSpeed 10
            stars <- initialState numStars maxSpeed
            simulateIO window background fps stars (pure . render) update
        _ -> print "Please supply --num <num>"

update :: ViewPort -> Float -> Starfield -> IO Starfield
update _ = moveStars
