module Main (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Time.Clock.System
import System.Posix.Unistd
import UI.NCurses

newtype SimState = SimState [Entity]

data Entity = Entity {
    tag :: Maybe Tag
  , tick :: Maybe Int
  , position :: Maybe (Int, Int)
  , velocity :: Maybe (Int, Int)
  , height :: Maybe Int
  , text :: Maybe String
  , ttfeed :: Maybe Int -- time to feed or die of starvation
  , ttdie :: Maybe Int -- time until life runs out
  , walk :: Maybe [Move]
  , input :: Maybe (Event -> Entity -> Entity)
  }

newEntity = Entity {
    tag = Nothing
  , tick = Nothing
  , position = Nothing
  , velocity = Nothing
  , height = Nothing
  , text = Nothing
  , ttfeed = Nothing  -- time to feed or die
  , ttdie = Nothing -- time until death
  , walk = Nothing
  , input = Nothing
  }

data Tag = Man | Food
    deriving Eq

data Move = N | W | E | S
    deriving Eq

tickInc :: Entity -> Entity
tickInc e@Entity{tick = Just t} = e{tick = Just (t+1)}
tickInc e = e

time :: IO Double
time = do
    t <- getSystemTime
    return ((fromIntegral (systemSeconds t)) + (fromIntegral (systemNanoseconds t)) * 1e-9)

-- A Simulation
data Simulation a = Simulation {
    dt :: Int         -- Physics simulation time step
  , frameRate :: Int  -- Max display frame rate
  , simState :: Maybe a
  , updateSim :: Maybe Event -> a -> Maybe a
  , renderSim :: (Integer, Integer) -> a -> Update ()
  }

newSimulation = Simulation {
    dt = 1
  , frameRate = 60
  , simState = Nothing
  , updateSim = undefined
  , renderSim = undefined
  }

data SimLoop = SimLoop {
    window :: Window         -- Current display window
  , nowTime :: Double        -- Current time
  , frameTime :: Double      -- Next frame will be displayed at this time
  , physicsTime :: Double    -- Physics has been simulated up to this time
  , fps :: Double            -- Current frame per second estimate
  }

collisionTop :: Entity -> Entity
collisionTop e@Entity{position = Just (x, y), velocity = Just (dx, dy)}
    | y <= 0 = e{position = Just (x, -y), velocity = Just (dx, -1*dy)}
    | otherwise = e
collisionTop e = e

collisionBot :: Entity -> Entity
collisionBot e@Entity{position = Just (x, y), velocity = Just (dx, dy), height = mh}
    | y + dy >= 40 = e{position = Just (x, y-(-1*dy)), velocity = Just (dx, -1*dy)}
    | otherwise = e
    --where h = fromMaddybe 0 mh
collisionBot e = e

collisionRight :: Entity -> Entity
collisionRight e@Entity{position = Just (x, y), velocity = Just (dx, dy)}
    | x <= 0 = e{position = Just (-x, y), velocity = Just (-1*dx, dy)}
    | otherwise = e
collisionRight e = e

collisionLeft :: Entity -> Entity
collisionLeft e@Entity{position = Just (x, y), velocity = Just (dx, dy)}
    | x >= 80 = e{position = Just (-x, y), velocity = Just (-1*dx, dy)}
    | otherwise = e
collisionLeft e = e


renderEntity :: (Integer, Integer) -> Entity -> Update ()
renderEntity (rows, cols) e@Entity{position = Just (x, y), height = Just h, text = Just t} = do
    moveCursor (fromIntegral x) (fromIntegral y)
    drawString t
renderEntity _ _ = return ()

renderWorld :: (Integer, Integer) -> SimState -> Update ()
renderWorld (rows, cols) (SimState es) = do
    --clear
    moveCursor 0 0
    --drawLineH Nothing cols
    --moveCursor (rows-1) 0
    --drawLineH Nothing cols
    --moveCursor 1 3
    drawString ("rows: " ++ show (rows) ++ " cols: " ++ show (cols)) 
    mapM_ (renderEntity (rows, cols)) es

integratePosition :: Entity -> Entity
integratePosition e@Entity{position = Just (x, y), velocity = Just (dx, dy)} = e{position = Just (x + dx, y + dy)}
integratePosition e = e


physics :: Simulation a -> SimLoop -> Curses (Simulation a, SimLoop)
physics s@Simulation{simState = Just ss} sl@SimLoop{nowTime = now} = do
    ev <- getEvent (window sl) (Just 0)
    t <- liftIO time
    return ( s{simState = (updateSim s) ev ss}, sl{nowTime = t} )
physics s sl = return (s, sl)

initWorld :: SimState
initWorld = SimState [
   newEntity{tag=Just Man,text= Just "O",position=Just (0,0),velocity=Just (1,1),height=Just 1,ttfeed=Just 50,ttdie=Just 100,walk=Just [N,W,S,S,E,W]}
   ]

-- Input Handlers
inputApply :: Maybe Event -> Entity -> Entity
inputApply (Just ev) e@Entity{input = Just f} = f ev e
inputApply _ e = e

updateWorld :: Maybe Event -> SimState -> Maybe SimState
updateWorld (Just (EventCharacter 'q')) _ = Nothing
updateWorld (Just (EventCharacter 'Q')) _ = Nothing
updateWorld ev (SimState es) = Just $ SimState (system es)
    where system = map collisionTop . map collisionBot . map collisionLeft . map collisionRight . map integratePosition . map tickInc . map (inputApply ev)

simLoop :: Simulation a -> SimLoop -> Curses ()
simLoop Simulation{simState = Nothing} sl = return ()
simLoop s@Simulation{simState = Just ss} sl = do
    size <- screenSize
    updateWindow (window sl) ((renderSim s) size ss)
    t <- liftIO time
    liftIO $ threadDelay $ 50000
    render
    ev <- getEvent (window sl) (Just 0)
    let (s', sl') = (s{simState = (updateSim s) ev ss}, sl{nowTime = t, frameTime = 0.0})
    simLoop s' sl'
   
runSimulation :: Simulation a -> IO ()
runSimulation s = runCurses $ do
    setCursorMode CursorInvisible
    setEcho False
    w <- defaultWindow
    t <- liftIO time
    simLoop s SimLoop{window = w, nowTime = t, frameTime = t, physicsTime = t, fps = 0.0}

-- Program entry point
main :: IO ()
main = runSimulation newSimulation{simState = Just initWorld, updateSim = updateWorld, renderSim = renderWorld}

