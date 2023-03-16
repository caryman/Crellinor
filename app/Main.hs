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

globalVars :: CrellinorConfig
globalVars = CrellinorConfig {
    ccEntitySpeed = 0
  , ccEntityCount = 1
  }

checkBoundary :: (Integer, Integer) -> Object -> Object
checkBoundary (rows, cols) o@Object{position1 = (x, y), position2 = (x2, y2), velocity = (dx, dy)}
    | (y <= 1 && dy < 0) || (y >= bottom && dy > 0) = o{velocity = (dx, -dy)}
    | (x <= 1 && dx < 0) || (x >= right && dx > 0) = o{velocity = (-dx, dy)}
    | otherwise = o
    where
       bottom = fromIntegral(rows-1)
       right = fromIntegral(cols-1)
checkBoundary _ o = o

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

--initWorld2 :: CrellinorConfig -> Objects
--initWorld2 c = do
--    g <- getStdGen
--    let x = [ newObect {

initWorld :: CrellinorConfig -> Objects
initWorld c = Objects [
   newObject {
       position1 = (0,0)
     , position2 = (0,0)
     , velocity  = (1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
   newObject {
       position1 = (14,15)
     , position2 = (15,14)
     , velocity  = (-1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [S,S,W,N,E,W]
     },
   newObject {
       position1 = (18,18)
     , position2 = (9,9)
     , velocity  = (-1,-1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
   newObject {
       position1 = (12,7)
     , position2 = (11,6)
     , velocity  = (1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
   newObject {
       position1 = (55,10)
     , position2 = (11,6)
     , velocity  = (1,-1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
   newObject {
       position1 = (70,12)
     , position2 = (11,6)
     , velocity  = (-1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
   newObject {
       position1 = (0,0)
     , position2 = (0,0)
     , velocity  = (1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
   newObject {
       position1 = (14,15)
     , position2 = (15,14)
     , velocity  = (-1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [S,S,W,N,E,W]
     },
   newObject {
       position1 = (44,5)
     , position2 = (9,9)
     , velocity  = (1,-1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
   newObject {
       position1 = (12,7)
     , position2 = (11,6)
     , velocity  = (1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
   newObject {
       position1 = (55,10)
     , position2 = (11,6)
     , velocity  = (1,-1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
   newObject {
       position1 = (10,2)
     , position2 = (11,6)
     , velocity  = (1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
    newObject {
       position1 = (17,24)
     , position2 = (11,6)
     , velocity  = (-1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
    newObject {
       position1 = (33,13)
     , position2 = (11,6)
     , velocity  = (1,-1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
     newObject {
       position1 = (0,0)
     , position2 = (11,6)
     , velocity  = (1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
    newObject {
       position1 = (12,12)
     , position2 = (11,6)
     , velocity  = (-1,-1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
    newObject {
       position1 = (44,2)
     , position2 = (11,6)
     , velocity  = (-1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
    newObject {
       position1 = (8,8)
     , position2 = (11,6)
     , velocity  = (1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     },
    newObject {
       position1 = (9,19)
     , position2 = (11,6)
     , velocity  = (-1,1)
     , text      = "o"
     , ttfeed    = 50
     , ttdie     = 100
     , walk      = [N,W,S,S,E,W]
     }
  ]

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
    let n = newObject {
        position1 = (getCol size, getRow size) :: (Int, Int)
      , position2 = (getCol size, getRow size) :: (Int, Int)
      , velocity  = (-1,1)
      , text      = "o"
      , ttfeed    = 50
      , ttdie     = 100
      , walk      = [N,W,S,S,E,W]
      }
    
    simLoop (initWorld c) c

-- Program entry point
main :: IO ()
main = do
    globalVars <- readConfig
    print $ (ccEntitySpeed globalVars)

    num <- randomIO :: IO Int
    print $ myPureFunction num
    g <- newStdGen
    let ns = randoms g :: [Int]
    print $ take 10 ns

    let (x, r) = randomR (0, 999) g
    print $ show (x :: Int)

    let (y, rg) = randomR ('A', 'Z') r
    print $ show y

    let c = randomRs (0, 999) rg :: [Int]
    mapM print . take 10 $ c

    x2 <- getStdRandom $ randomR (1,999) 
    print $ show (x2 :: Int)

    let x3 = getRIntIO (0,99) 5

    gen <- getStdGen
    let xs = randoms gen :: [Int]
    print $ take 10 xs

    gn <- getStdGen
    let x5 = getRInt gn (0,999) 10
    mapM print $ x5
    print $ "typeof x5: " ++ show (typeOf x5) 

    runSimulation globalVars

getRow :: (Integer, Integer) -> Int
getRow (rows, cols) = getARInt rows :: Int

getCol :: (Integer, Integer) -> Int
getCol (rows, cols) = getARInt cols :: Int

getARInt :: Integer -> Int
getARInt ubound = getStdRandom $ randomR (0, 999) :: Int

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

