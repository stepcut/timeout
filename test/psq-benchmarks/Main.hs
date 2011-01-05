{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Trans
import           Criterion
import           Criterion.Config hiding (Plot)
import           Criterion.Environment
import           Criterion.Measurement
import           Criterion.Monad
import           Data.Accessor
import           Data.Colour
import           Data.Colour.Names
import           Data.List
import           Data.Maybe
import qualified Data.PSQueue as PSQ
import           Data.PSQueue (PSQ, Binding(..))
import qualified Data.Vector as V
import           Data.Vector (Vector)
import qualified Data.Vector.Algorithms.Merge as V
import           Graphics.Rendering.Chart hiding (Vector)
import           Graphics.Rendering.Chart.Gtk
import           Graphics.Rendering.Chart.Plot
import           Statistics.Distribution
import           Statistics.Distribution.Exponential
import           Statistics.Sample
import           System.Environment
import           System.IO.Unsafe
import           System.Random.MWC


-- attempt to model the behaviour of the timeout psq we're using. "threads"
-- will twiddle the PSQ and a reaper will periodically delete the expired
-- threads. First we'll set up the PSQ type.
type Table = PSQ Int Int

-- Next we'll set up "commands" for our simulation.

data Command = ReapOlderThan Int    -- cause a "reaping" to happen
             | NewThread Int Int    -- new thread with a given timeout
             | UpdateThread Int Int -- update a thread's timeout value
             | KillThread Int       -- delete the thread from the queue
  deriving (Eq, Ord, Show)

-- Need a deepseq instance so that the simulation stream is fully evaluated
-- when we benchmark the PSQ
instance NFData Command where
    rnf (ReapOlderThan x)  = rnf x
    rnf (NewThread x y)    = rnf x `seq` rnf y `seq` ()
    rnf (KillThread x)     = rnf x
    rnf (UpdateThread x y) = rnf x `seq` rnf y `seq` ()

-- Next we need something to tag events with a faux timestamp so we can
-- "replay" the event stream
newtype Event = Event { unEvent :: (Int, Command) }
  deriving (Eq, Ord, NFData, Show)


-- And we need a function to apply a Command to the table
applyEvent :: Command -> Table -> Table
applyEvent cmd !psq = go cmd
  where
    go (ReapOlderThan x)  = reap psq x
    go (NewThread t x)    = PSQ.insert t x psq
    go (KillThread t)     = PSQ.delete t psq
    go (UpdateThread t x) = PSQ.insert t x psq

    reap !tab x = maybe tab del $ PSQ.findMin tab
      where
        del (tid :-> prio) =
            if prio < x then reap (PSQ.deleteMin tab) x else tab


-- Now for our simulation we need to mock the behaviour of threads.

-- The reaper thread generates an infinite stream of ReapOlderThan events.
reaperThread :: Int   -- ^ start time
             -> Int   -- ^ how often to run
             -> Int   -- ^ timeout value
             -> [Event]
reaperThread st freq timeout = events
  where
    events = unfoldr f st
    f t = Just (Event (t, ReapOlderThan (t-timeout)), t+freq)


-- And a simulation of a request thread.


requestThread :: GenIO    -- ^ RNG
              -> Int      -- ^ timeout value
              -> Int      -- ^ start time
              -> Int      -- ^ thread id
              -> IO (Vector Event)
requestThread rng timeout startTime tid = do
    numIOs  <- uniformR (1,4) rng
    ioTimes <- V.replicateM numIOs $ ioTime rng (exponential 2.0)
                                   $ fromIntegral timeout
    let initialTimeout = startTime + timeout
    let eventTimes     = mkEventTimes ioTimes
    let timeouts       = V.map (+ timeout) eventTimes
    let killTime       = startTime + V.sum ioTimes
    let ioEvents       = eventTimes `V.zip` V.map (UpdateThread tid) timeouts

    let xs = V.concat
               [ V.fromList [(startTime, NewThread tid initialTimeout)]
               , ioEvents
               , V.fromList [(killTime, KillThread tid)] ]

    return $ V.map Event xs

  where
    mkEventTimes = V.fromList . reverse . snd . V.foldl' f (startTime,[])
      where
        f (t,l) u = let v = t+u in (v,v:l)

    -- we'll make IO activity follow an exponential distribution, most IO will be
    -- very short but some will be much longer. Make IO always last at least one
    -- second though
    ioTime rng dist range = do
        x <- uniformR (0.3, 6.0) rng
        return $ max 1
               $ min (round range)
               $ round
               $ density dist x * range


-- Code to generate an infinite stream of events, given that N threads are
-- created every second.
eventStream :: Int -> IO [Vector Event]
eventStream nPerSec = do
    rng <- create
    unsafeInterleaveIO $ go rng V.empty (reaperThread 0 5 timeout) 0 0

  where
    vsort v = do
        mv <- V.unsafeThaw v
        V.sort mv
        V.unsafeFreeze mv

    timeout = 10
    splitEventsAt y = span (\x -> fst (unEvent x) <= y)
    vSplitEventsAt y = V.span (\x -> fst (unEvent x) <= y)

    go rng eventsInQueue reaperEvents currentTimeValue maxTid = do
        let maxTid' = maxTid + nPerSec
        newEvents <- mapM (requestThread rng timeout currentTimeValue)
                          [maxTid..(maxTid'-1)] >>=
                     vsort . V.concat
        let (eventQ, eventRest)   = vSplitEventsAt currentTimeValue eventsInQueue
        let (newQ, newRest)       = vSplitEventsAt currentTimeValue newEvents
        let (reaperQ, reaperRest) = splitEventsAt currentTimeValue reaperEvents

        thisSecondEvents <- vsort $ V.concat [ eventQ
                                             , newQ
                                             , V.fromList reaperQ ]
        remainingQueue <- vsort $ V.concat [eventRest, newRest]

        outputStream <- unsafeInterleaveIO $
                        go rng remainingQueue reaperRest (currentTimeValue+1)
                           maxTid'
        return $ thisSecondEvents:outputStream


mkEvents nPerSec nSecs = do
    putStrLn $ concat [ "making event stream, "
                      , show nPerSec
                      , " qps for "
                      , show nSecs
                      , " seconds" ]
    stream <- eventStream nPerSec
    let !events = takeUntilNSecs stream

    putStrLn $ "event stream complete"
    return $! events

  where
    takeUntilNSecs [] = []
    takeUntilNSecs (z:zs) =
        if V.null b
          then z:takeUntilNSecs zs
          else [a]
      where
        !_ = V.foldl' (\c d -> c `deepseq` d `deepseq` ()) () a
        (a,b) = V.span (\x -> fst (unEvent x) <= nSecs) z


computeTable events = go PSQ.empty events
  where
    go !tab [] = tab
    go !tab (v:vs) = go tab' vs
      where
        !tab' = V.foldl' (\t e -> applyEvent (snd $ unEvent e) t) tab v


killAll !tab = maybe tab (const $ killAll $ PSQ.deleteMin tab) mm
  where
    mm = PSQ.findMin tab


takeSample :: Environment
           -> Int   -- ^ qps
           -> Criterion (Int, Int, Sample)
takeSample env qps = do
    events <- liftIO $ mkEvents qps 15
    let nOps = foldl' (\a v -> a + V.length v) 0 events
    let proc = whnf computeTable events
    sample <- runBenchmark env proc 
    return (qps, nOps, sample)

    
takeSamples :: Environment -> Criterion [(Int, Int, Sample)]
takeSamples env = mapM (takeSample env) [500,1000,2000,4000,8000]


plotSamples :: String
            -> CairoLineStyle
            -> [(Int, Int, Sample)]
            -> Plot Int Double
plotSamples name lineStyle samples = toPlot plot
  where
    value (x,nops,sample) = symErrPoint x v 0 s
      where
        nopsReal :: Double
        nopsReal = fromIntegral nops

        (meanValue, variance) = meanVarianceUnb sample
        stddev = sqrt $ abs variance

        v = meanValue / nopsReal
        s = stddev / nopsReal

    plot = plot_errbars_title  ^= name
         $ plot_errbars_values ^= map value samples
         $ plot_errbars_line_style ^= lineStyle
         $ defaultPlotErrBars


chart :: Double
      -> [(Int, Int, Sample)]
      -> Renderable ()
chart lwidth samples = toRenderable layout
  where
    lineStyle = line_width ^= 3*lwidth
              $ line_color ^= opaque blue
              $ defaultPlotErrBars ^. plot_errbars_line_style

    plot = plotSamples "PSQ" lineStyle samples

    remapLabels = axis_labels ^: f
      where
        f labels = map (map g) labels
        g (x,_) = (x, secs x)

    layout = layout1_title ^= "PSQ per-operation timing"
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_left_axis ^: laxis_override ^= remapLabels
           $ layout1_plots ^= [Left plot]
           $ defaultLayout1


main = do
    samples <- withConfig config $ measureEnvironment >>= takeSamples
    getArgs >>= main1 samples
  where
    config = defaultConfig { cfgSamples = ljust 5 }

main1 samples [] = do
    renderableToWindow (chart 1.00 samples) 1024 768 
    return ()

main1 samples (fn:_) = do
    renderableToPNGFile (chart 0.25 samples) 800 600 fn
    return ()
