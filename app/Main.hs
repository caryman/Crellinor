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
  , position :: Maybe (Int, Int)
  , text :: Maybe String
  , ttfeed :: Maybe Int -- time to feed or die of starvation
  , ttdie :: Maybe Int -- time until life runs out
  , walk :: Maybe [Move]
  }

newEntity = Entity {
    tag = Nothing
  , position = Nothing
  , text = Nothing
  , ttfeed = Nothing  -- time to feed or die
  , ttdie = Nothing -- time until death
  , walk = Nothing
  }

data Tag = Blob | Food
    deriving Eq

data Move = N | W | E | S
    deriving Eq

-- Initial World
initWorld :: SimState
initWorld = SimState [
    newEntity {
        tag = Just Blob
      , position = Just (4, 3)
      , text = Just "O"
      , ttfeed = Just 12
      , ttdie = Just 100
      , walk = Just [W,S,N,N,E,S]
        }
   ]


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
  , renderSim :: (Integer, Integer) -> Double -> Update ()
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

physics :: Simulation a -> SimLoop -> Curses (Simulation a, SimLoop)
physics g@Simulation{dt = dt, gameState = Just gs} gl@SimLoop{nowTime = now, physicsTime = pt, frameTime = ft}
    | pt + 1.0 < ft = physics g gl{physicsTime = now}  -- Skip ahead after missing time
    | pt < ft = do
        when (now < pt) (liftIO $ threadDelay (truncate (1e6 * clamp (ft - pt) 0.0 dt)))
        ev <- getEvent (window gl) (just 0)
        physics s{simState = (updateSimulation g) ev gs} sl{physicsTime = pt + dt}
physics s sl = return (s, sl)

simLoop :: Simulation a -> SimLoop -> Curses ()
simLoop Simulation{simState = Nothing} sl = return ()
simLoop s@Simulation{simState = Just ss} sl = do
    size <- screensize
    updateWindow (window sl) ((renderSimulation s) size (fps sl) ss)
    render
    t <- liftIO time
    (s', sl') <- physics s sl{nowTime = t, frame
   

runSimulation :: Simulation a -> IO ()
runSimulation g = runCurses $ do
    setCursorMode CursorInvisible
    setEcho False
    w <- defaultWindow
    t <- liftIO time
    simLoop g SimLoop{window = w, nowTime = t, frameTime = t, physicsTime = t, fps = 0.0}

-- Program entry point
main :: IO ()
main = runSimulation newSim{simState = Just initWorld, updateSim = updateWorld, renderSim = renderWorld}


