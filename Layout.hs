{-# LANGUAGE RecordWildCards #-}

module Layout
  ( FinishedLine
  , MeasuredLine
  , em
  , findOptimalBreaks
  , unWidth
  , width
  ) where

import Control.Monad
import Control.Monad.State
import Data.Function (on)
import Data.List
import Data.Maybe
import Diagrams.Prelude (def)
import Graphics.SVGFonts.PathInRect (PathInRect(..))
import Graphics.SVGFonts.ReadFont (PreparedFont)
import qualified Data.Vector as V
import qualified Graphics.SVGFonts as F

import Debug.Trace

data Range = Range
  { begin :: Int
  -- ^ The index of the first element in the range (inclusive).
  , end :: Int
  -- ^ One plus the index of the last element in the range
  -- (exclusive).
  }

em :: Double
em = 22

-- | The type of things with a width.
data Widthed a = Widthed Double a
  deriving (Show)

unWidth :: Widthed a -> a
unWidth (Widthed _ a) = a

width :: Widthed a -> Double
width (Widthed w _) = w

data Glue = Glue
  { idealWidth     :: Double  -- w_i
  , stretchability :: Double  -- y_i
  , shrinkability  :: Double  -- z_i
  }
  deriving (Show)

type MeasuredLine = [(Widthed String, Glue)]
type FinishedLine = [(Widthed String, Widthed Glue)]

class MeasureLength a where
  measureLength :: a -> Double

instance MeasureLength (Widthed a) where
  measureLength (Widthed w _) = w

instance MeasureLength Glue where
  measureLength Glue { idealWidth } = idealWidth

instance (MeasureLength a, MeasureLength b) => MeasureLength (a, b) where
  measureLength (a, b) = measureLength a + measureLength b

instance MeasureLength a => MeasureLength (V.Vector a) where
  measureLength = sum . V.map measureLength

-- | Keep a prefix-sum of the ideal width upto each box, along with
-- the total stretchability and shrinkability. This allows us to query
-- the total width/stretchability/shrinkability for a range of the
-- paragraph in constant time (see "accumSum").
data Accumulators = Accumulators
  { widthAccum :: V.Vector Double
  , stretchabilityAccum :: V.Vector Double
  , shrinkabilityAccum :: V.Vector Double
  }
  deriving (Show)

mkAccumulators :: MeasuredLine -> Accumulators
mkAccumulators l = Accumulators
  { widthAccum = toAccumulator boxWidths
  , stretchabilityAccum = toAccumulator stretchabilities
  , shrinkabilityAccum = toAccumulator shrinkabilities
  }
  where
    boxWidths = map (\(b, g) -> width b + idealWidth g) l
    stretchabilities = map (stretchability . snd) l
    shrinkabilities = map (shrinkability . snd) l

    toAccumulator = V.fromList . toAccumulator' 0

    toAccumulator' _ [] = []
    toAccumulator' s (a:as) = s + a:toAccumulator' (s + a) as

totalStretchability :: Accumulators -> Range -> Double
totalStretchability = accumSum . stretchabilityAccum

totalShrinkability :: Accumulators -> Range -> Double
totalShrinkability = accumSum . shrinkabilityAccum

totalWidth :: Accumulators -> Range -> Double
totalWidth = accumSum . widthAccum

-- | Find the sum of all elements in the range, over the given
-- accumulator.
accumSum :: V.Vector Double -> Range -> Double
accumSum v Range{ begin = 0, end = 0 } = 0
accumSum v Range{ begin = 0, end } = v V.! (end - 1)
accumSum v Range{..} = v V.! (end - 1) - v V.! (begin - 1)

-- | Calculate the adjustment ratio of a line. The adjustment ratio is
-- positive if the line is shorter than desired, and is negative if
-- the line is longer than desired. A value of 0 means that the line
-- is already the perfect length.
--
-- Note that if r < -1, then there will never be enough shrinkability
-- to layout the line.
adjustmentRatio :: Double -> Accumulators -> Range -> Double
adjustmentRatio desiredWidth acc range
  | w < desiredWidth = (desiredWidth - w) / totalStretchability acc range
  | otherwise = (desiredWidth - w) / totalShrinkability acc range
  where
    w = totalWidth acc range

defaultGlue :: Glue
defaultGlue = Glue
  { idealWidth = em / 3
  , stretchability = em / 6
  , shrinkability = em / 9
  }

finishingGlue :: Glue
finishingGlue = Glue
  { idealWidth = 0
  , stretchability = 10000
  , shrinkability = 0
  }

-- | Set the final width of `Glue` given the adjustment ratio of the
-- line.
widthGlue :: Double -> Glue -> Widthed Glue
widthGlue r g
  | r >= 0 = Widthed (idealWidth g + r * stretchability g) g
  | otherwise = Widthed (idealWidth g + r * shrinkability g) g

-- | Apply a function to the second element of a tuple.
second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

finishLine :: Double -> MeasuredLine -> FinishedLine
finishLine r = map (second (widthGlue r))

badness :: Double -> Double
badness r
  | r < -1 = 10000
  | otherwise = 100 * (abs r ** 3)

type WidthedLine = V.Vector (Widthed String, Widthed Glue)

widthString :: PreparedFont Double -> String -> Widthed String
widthString font s = Widthed width s
  where
    width = scale * (x2 - x1)
    PathInRect x1 y1 x2 y2 _ = F.svgText def{F.textFont = font} s
    scale = em / (y2 - y1)

-- | The `BreakNode` represents a possible line break.
data BreakNode = BreakNode
  { parent :: Maybe BreakNode
  , cost :: Double
  , index :: Int
  , r :: Double
  }

instance Show BreakNode where
  show BreakNode {..} =
    show index ++ "@" ++ show cost

data LayoutState = LayoutState
  { activeBreaks :: [BreakNode]
  , accumulators :: Accumulators
  }
  deriving (Show)

type LayoutM a = State LayoutState a

forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM as f = catMaybes <$> mapM f as

-- | Add breakpoints before the word `i` to the set of activeBreaks if
-- they are feasible.
--
-- A breakpoint is considered feasible if a line from any of the
-- active breaks to this break has an acceptable adjustment ratio.
addFeasible :: Double -> Int -> LayoutM ()
addFeasible l i = do
  breaks <- gets activeBreaks
  candidates <- forMaybeM breaks $ \a@BreakNode { .. } -> do
    acc <- gets accumulators
    let r = adjustmentRatio l acc (Range index i)
    -- Test if r is acceptable. These bounds can be tweaked to allow
    -- tighter or looser lines.
    pure $ if r > -1 && r < 1 then
      Just (BreakNode (Just a) (cost + badness r) i r)
      else
      Nothing
  -- Note that all of the `candidates` at this point have the same
  -- parent. Since the cost of each parent is factored into the cost
  -- of each candidate, we know with certainity that only the lowest
  -- cost candidate at this point will yield an optimal layout.
  if null candidates then pure ()
    else let best = minimumBy (compare `on` cost) candidates
    in modify (\s -> s { activeBreaks = best:breaks })

-- | Remove all breakpoints from the active list for which a line
-- starting at the breakpoint and ending just before `i` has an
-- adjustment ratio less than -1.
filterInfeasible :: Double -> Int -> LayoutM ()
filterInfeasible l i = do
  breaks <- gets activeBreaks
  breaks' <- forMaybeM breaks $ \a@BreakNode { .. } -> do
    acc <- gets accumulators
    let r = adjustmentRatio l acc (Range index i)
    pure $ if r > -1 then
      Just a
      else
      Nothing
  modify (\s -> s { activeBreaks = breaks' })

-- | Find the best locations to break the text.
findOptimalBreaks :: PreparedFont Double -> String -> Double -> [FinishedLine]
findOptimalBreaks _  "" _ = []
findOptimalBreaks font text l = breakTextAtPoints withGlue bestBreak
  where
    bestBreak = minimumBy (compare `on` cost) (activeBreaks finalState)

    finalState = execState go initialState

    go = forM [0..length withGlue - 1] $ \i ->
      addFeasible l i >> filterInfeasible l i

    initialState = LayoutState
      { activeBreaks = [BreakNode Nothing 0 0 0]
      , accumulators = mkAccumulators withGlue
      }
    withGlue =
      map ((, defaultGlue) . widthString font) (init ws)
      ++ [(widthString font (last ws), finishingGlue)]

    ws = words text

-- | Break the text into lines based on the given BreakNode.
breakTextAtPoints :: MeasuredLine -> BreakNode -> [FinishedLine]
breakTextAtPoints ws = reverse . go ws 0
  where
    -- Note that we take `r` as a parameter here since we need to
    -- effectively shift values of `r` backwards by one step here as
    -- we form `FinishedLine`s. The `r` value assigned to a
    -- `BreakNode` is the value of `r` for the line _ending_ at the
    -- `BreakNode`, but when we encounter a `BreakNode` in this
    -- function, we dispatch the line _starting_ at the `BreakNode`.
    go :: MeasuredLine -> Double -> BreakNode -> [FinishedLine]
    go ws r (BreakNode Nothing _ i r') = [finishLine r ws]
    go ws r (BreakNode (Just p) _ i r') = finishLine r lastLine:go rest r' p
      where
        (rest, lastLine) = splitAt i ws
