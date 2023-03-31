module Main (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Time.Clock.System
import System.Posix.Unistd
import UI.NCurses
import System.IO
import System.Directory
import Data.Char
import System.Random
import Data.Typeable(typeOf)
import Config
import Debug.Trace

newtype Objects = Objects [Object]
data Position = Position { x :: Int, y :: Int } deriving Show
data Velocity = Velocity { dx :: Int, dy :: Int } deriving Show

data Object = Object {
    position1 :: Position 
  , position2 :: Position
  , velocity :: Velocity
  , text :: String
  , ttfeed :: Int -- time to feed or die of starvation
  , ttdie :: Int  -- time until life runs out
  , walk :: [Move]
  } deriving Show

getRandomPosition :: Int -> Int -> IO Position
getRandomPosition rows cols = do
  randomX <- randomRIO (1, fromIntegral(cols))
  randomY <- randomRIO (1, fromIntegral(rows))
  return $ Position randomX randomY

getRandomPosition2 :: Position -> IO Position
getRandomPosition2 p@Position{x=x1,y=y1} = do
  return $ Position x1 y1

getRandomVelocity :: IO Velocity
getRandomVelocity = do
  randomX <- randomRIO (0, 2)
  randomY <- randomRIO (0, 2)
  return $ Velocity (randomX - 1) (randomY - 1)

getRandomObject :: Int -> Int -> IO Object
getRandomObject rows cols = do
  randomPosition1 <- liftIO $ getRandomPosition rows cols
  randomVelocity <- liftIO getRandomVelocity
  randomPosition2 <- getRandomPosition2 randomPosition1
  return $ Object randomPosition1 randomPosition2 randomVelocity "o" 50 100 [E, S, E, N, W, S]

getRandomObjects :: Int -> Int -> IO Objects
getRandomObjects rows cols = Objects <$> replicateM 60 (getRandomObject rows cols)

newObject = Object {
    position1 = Position {x = 1, y = 1}
  , position2 = Position {x = 0, y = 0}
  , velocity = Velocity {dx = 1, dy = 0}
  , text = "o"
  , ttfeed = 50  -- time to feed or die
  , ttdie = 100  -- time until death
  , walk = [E, S, E, N, W, S]
  }

data Move = N | W | E | S
    deriving (Eq, Show)

globalVars :: CrellinorConfig
globalVars = CrellinorConfig {
    ccEntitySpeed = 0
  , ccEntityCount = 1
  }

checkBoundary :: (Integer, Integer) -> Object -> Object
checkBoundary (rows, cols) o@Object{position1 = Position {x=x1, y=y1}, position2 = Position {x=x2, y=y2}, velocity = Velocity {dx=dx1, dy=dy1}}
    | (y1 <= 1 && dy1 < 0) || (y1 >= bottom && dy1 > 0) = o{velocity = Velocity {dx=dx1, dy=(-dy1)}}
    | (x1 <= 1 && dx1 < 0) || (x1 >= right && dx1 > 0) = o{velocity = Velocity {dx=(-dx1), dy=dy1}}
    | otherwise = o
    where
       bottom = fromIntegral(rows-1)
       right = fromIntegral(cols-1)
checkBoundary _ o = o

renderObject :: (Integer, Integer) -> Object -> Update ()
renderObject (rows, cols) o@Object{position1 = Position {x=x1, y=y1}, position2 = Position {x=x2,y=y2}, text = t} = do
    moveCursor (fromIntegral y1) (fromIntegral x1)
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
updatePosition o@Object{position1 = Position {x=x1, y=y1}, position2 = Position {x=x2, y=y2}, velocity = Velocity {dx=dx1, dy=dy1}} = o{position1 = Position {x = (x1 + dx1), y = (y1 + dy1)}, position2 = Position {x=x1, y=y1}}
updatePosition o = o


updateWorld :: (Integer, Integer) -> Objects -> Objects
updateWorld (rows, cols) (Objects objs) = Objects (system objs)   
    where system = map updatePosition . map (checkBoundary (rows, cols))

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


simLoop :: Objects -> CrellinorConfig -> Curses ()
simLoop objs c = do
    size <- screenSize
    w <- defaultWindow
    updateWindow w (renderWorld size objs)
    liftIO $ threadDelay $ (ccEntitySpeed c) -- 50000
    render
    k <- checkKeyboard
    if k == Nothing 
        then return ()
    else
       simLoop (updateWorld size objs) c

runSimulation :: CrellinorConfig -> IO ()
runSimulation c = runCurses $ do
    setCursorMode CursorInvisible
    setEcho False
    size <- screenSize
    rows <- liftIO $ getRow size
    cols <- liftIO $ getCol size
    (Objects objs) <- liftIO $ getRandomObjects rows cols
    simLoop (Objects objs) c

--drawObj :: Object -> IO ()
--drawObj o@Object{position1=p1} = do 
--    drawString("col: " ++ show(p1)) 
--    print ""

loop :: Curses ()
loop = do
    k <- checkKeyboard
    if k == Nothing
        then return ()
    else
        loop

printObjs :: [Object] -> IO ()
printObjs o = do
    print $ "printing objects"
    mapM_ print o
    

-- Program entry point
main :: IO ()
main = do
    globalVars <- readConfig
    print $ (ccEntitySpeed globalVars)
    void $ runSimulation globalVars


getRow :: (Integer, Integer) -> IO Int
getRow (rows, _) = getARInt rows

getCol :: (Integer, Integer) -> IO Int
getCol (_, cols) = getARInt cols

rollDice :: IO Int
rollDice = getStdRandom $ randomR (1, 6)

getARInt :: Integer -> IO Int
getARInt ubound = getStdRandom $ randomR (0, fromIntegral(ubound))

getRInt :: (RandomGen g) => g -> (Int,Int) -> Int -> [Int]
getRInt r (lbound, ubound) amt = take amt $ randomRs (lbound,ubound) r

getRIntIO :: (Int,Int) -> Int -> IO [Int]
getRIntIO (lbound, ubound) amt = do
    r1 <- getStdGen
    let x = randomRs (lbound,ubound) r1
    return x

quit :: Curses ()
quit = do
    w <- defaultWindow
    updateWindow w $ do
        clear
    render

-- nothing to say
readConfig :: IO CrellinorConfig
readConfig = do
    path <- System.Directory.getCurrentDirectory
    let fileNameAndPath = path ++ "/" ++ fileName
    print(fileNameAndPath)
    ymlData <- readConfigurationGeneric fileNameAndPath :: IO CrellinorConfig
    print $ ymlData
    writeConfiguration fileNameAndPath ymlData
    newConfig <- newConfiguration fileNameAndPath :: IO Configuration
    print $ configurationPath newConfig
    mv <- takeMVar (configurationVar newConfig)
    return mv

myPureFunction :: Int -> Int
myPureFunction x = 2 * x

