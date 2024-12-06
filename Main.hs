module Main where

import Control.Monad
import Control.Monad.State
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Graphics.SVGFonts.ReadFont (PreparedFont)
import Layout
import Numeric
import qualified Graphics.SVGFonts as F

t :: String
t = unwords [ "These H-gadgets are inserted between each fragment"
            , "on a line (as opposed to V-gadgets, which are inserted"
            , "between lines). Once the h-gadgets have been inserted"
            , "the length of each line can be found by summing the"
            , "width of each fragment, along with the width of its"
            , "abutting H-gadgets."
            , "  After width resolution, it's time to resolve the height"
            , "of each line. So called height resolution is accomplished"
            , "using V-gadgets, which are analagous to H-gadgets,"
            , "but exist between lines as opposed to between fragments."
            , "These V-gadgets are inserted based on the horizontal"
            , "positions of the H-gadgets found in the previous phase." ]

w :: Double
w = 500

nextIndex :: State Int Int
nextIndex = do
  n <- gets id
  modify (+1)
  pure n

formatDouble :: Double -> String
formatDouble d = showFFloat (Just 3) d ""

line :: PreparedFont Double -> FinishedLineWithAdj -> State Int (Diagram B)
line font (line, r) = go 0 line
  where
    go :: Double -> FinishedLine -> State Int (Diagram B)
    go x [] =
      pure $ (F.drop_rect . F.fit_height (0.75 * em) $ F.svgText def{F.textFont = font} (formatDouble r))
      # stroke
      # lw none
      # fc blue
      # translateX (max w x + 10.0)
    go x (w:ws) = do
      i <- nextIndex
      let word =
            (F.drop_rect . F.fit_height (1 * em) $ F.svgText def{F.textFont = font} (unWidth . fst $ w))
            # stroke
            # lw none
            # fc black
            # translateX x
      let indexLabel =
            (F.drop_rect . F.fit_height (0.5 * em) $ F.svgText def{F.textFont = font} (show i))
            # stroke
            # lw none
            # fc red
            # translateX x
            # translateY (0.5 * em)
      let glue =
            rect glueWidth (0.5 * em)
            # lw none
            # fc lightgray
            # translateY (0.25 * em)
            # translateX (x + wordWidth + (glueWidth / 2))
      rest <- go (x + wordWidth + glueWidth) ws
      pure $ mconcat [word, indexLabel, glue, rest]
      where
        wordWidth = Layout.width (fst w)
        -- If we wanted ragged-right text, we could just set the
        -- `glueWidth` to its idealWidth (1/3 em).
        glueWidth = if null ws then 0 else Layout.width (snd w)

paragraph :: PreparedFont Double -> String -> Diagram B
paragraph font text = mconcat $ evalState (zipWithM f lines [0..]) 0
  where
    f ws lineNo = do
      d <- line font ws
      pure $ d # translateY (lineNo * em * (-1.1))
    lines = findOptimalBreaks font text w

main :: IO ()
main = do
  font <- F.loadFont "./font/LiberationSerif.svg"
  let
    diagram :: Diagram B
    diagram = paragraph font t <> vrule 100 # moveOriginBy (r2 (0, 50)) # translateX w
  mainWith diagram
