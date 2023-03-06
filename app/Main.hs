module Main (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Time.Clock.System
import System.Posix.Unistd
import UI.NCurses

newtype Objects = Objects [Object]

data Object = Object {
    position1 :: (Int, Int)
  , position2 :: (Int, Int)
  , velocity :: (Int, Int)
  , text :: String
  , ttfeed :: Int -- time to feed or die of starvation
  , ttdie :: Int  -- time until life runs out
  , walk :: [Move]
  }

newObject = Object {
    position1 = (1, 1)
  , position2 = (0, 0)
  , velocity = (1, 0)
  , text = "o"
  , ttfeed = 50  -- time to feed or die
  , ttdie = 100  -- time until death
  , walk = [E, S, E, N, W, S]
  }

data Move = N | W | E | S
    deriving Eq

checkBoundary :: (Integer, Integer) -> Object -> Object
checkBoundary (rows, cols) o@Object{position1 = (x, y), position2 = (x2, y2), velocity = (dx, dy)}
    | (y <= 0 && dy < 0) || (y >= bottom && dy > 0) = o{velocity = (dx, -dy)}
    | (x <= 0 && dx < 0) || (x >= right && dx > 0) = o{velocity = (-dx, dy)}
    | otherwise = o
    where
       bottom = fromIntegral(rows-1)
       right = fromIntegral(cols-1)
collisionTop o = o

renderObject :: (Integer, Integer) -> Object -> Update ()
renderObject (rows, cols) o@Object{position1 = (x, y), position2 = (x2, y2), text = t} = do
    moveCursor 5 20
    drawString "               "
    moveCursor 5 20
    drawString ("x: " ++ show (x) ++ " y: " ++ show (y)) 
    moveCursor (fromIntegral y) (fromIntegral x)
    drawString t
    moveCursor (fromIntegral y2) (fromIntegral x2)
    drawString " "
renderObject _ _ = return ()

renderWorld :: (Integer, Integer) -> Objects -> Update ()
renderWorld (rows, cols) (Objects objs) = do
    --clear
    moveCursor 2 10
    drawString ("rows: " ++ show (rows) ++ " cols: " ++ show (cols)) 
    mapM_ (renderObject (rows, cols)) objs

updatePosition :: Object -> Object
updatePosition o@Object{position1 = (x, y), position2 = (x2, y2), velocity = (dx, dy)} = o{position1 = (x + dx, y + dy), position2 = (x, y)}
updatePosition o = o

initWorld :: Objects
initWorld = Objects [
   newObject {
       position1 = (0,0)
     , position2 = (0,0)
     , velocity  = (1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
       }
   ]

updateWorld :: (Integer, Integer) -> Objects -> Objects
updateWorld (rows, cols) (Objects objs) = Objects (system objs)   
    where system = map (checkBoundary (rows, cols)) . map updatePosition

data KeyAction = NoAction | Quit | LPaddleUp | LPaddleDn | RPaddleUp | RPaddleDn | Restart | Pause | Faster | Slower deriving (Eq, Show) 

checkKeyboard :: Curses (Maybe KeyAction)
checkKeyboard = do
    w <- defaultWindow
    e <- getEvent w (Just 0)
    return $ case e of
       Nothing -> Just NoAction
       Just (EventCharacter 'q') -> Nothing
       --Just (EventCharacter 'r') -> Just Restart
       --Just (EventCharacter 'k') -> Just RPaddleUp
       --Just (EventCharacter 'l') -> Just RPaddleDn
       --Just (EventCharacter 'a') -> Just LPaddleUp
       --Just (EventCharacter 's') -> Just LPaddleDn
       --Just (EventCharacter 'p') -> Just Pause
       --Just (EventCharacter '-') -> Just Slower
       --Just (EventCharacter '=') -> Just Faster
       Just _ -> Just NoAction

simLoop :: Objects -> Curses ()
simLoop objs = do
    size <- screenSize
    w <- defaultWindow
    updateWindow w (renderWorld size objs)
    liftIO $ threadDelay $ 50000
    render
    k <- checkKeyboard
    if k == Nothing 
        then return ()
    else
       simLoop $ updateWorld size objs
   
runSimulation :: IO ()
runSimulation = runCurses $ do
    setCursorMode CursorInvisible
    setEcho False
    simLoop initWorld

-- Program entry point
main :: IO ()
main = runSimulation

quit :: Curses ()
quit = do
    w <- defaultWindow
    updateWindow w $ do
        clear
    render

