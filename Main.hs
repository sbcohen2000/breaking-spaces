module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Graphics.SVGFonts.PathInRect (PathInRect(..))
import Graphics.SVGFonts.ReadFont (PreparedFont)
import Layout
import qualified Graphics.SVGFonts as F

t :: String
t = "During much of the letterpress era, movable type was composed by hand for each page by workers called compositors. A tray with many dividers, called a case, contained cast metal sorts, each with a single letter or symbol, but backwards (so they would print correctly). The compositor assembled these sorts into words, then lines, then pages of text, which were then bound tightly together by a frame, making up a form or page. If done correctly, all letters were of the same height, and a flat surface of type was created. The form was placed in a press and inked, and then printed (an impression made) on paper.[3] Metal type read backwards, from right to left, and a key skill of the compositor was their ability to read this backwards text."

line :: PreparedFont Double -> FinishedLine -> Diagram B
line font = go 0
  where
    go :: Double -> FinishedLine -> Diagram B
    go _ [] = mempty
    go x (w:ws) =
      (F.drop_rect . F.fit_height (1 * em) $ F.svgText def{F.textFont = font} (unWidth . fst $ w))
      # stroke
      # lw none
      # fc black
      # translateX x
      <> go (x + wordWidth + glueWidth) ws
      where
        wordWidth = Layout.width (fst w)
        -- If we wanted ragged-right text, we could just set the
        -- `glueWidth` to its idealWidth (1/3 em).
        glueWidth = Layout.width (snd w)

paragraph :: PreparedFont Double -> String -> Diagram B
paragraph font text = mconcat $ zipWith f lines [0..]
  where
    f ws lineNo =
      line font ws
      # translateY (lineNo * em * (-1.1))
    lines = findOptimalBreaks font text 500

main :: IO ()
main = do
  font <- F.loadFont "./font/LiberationSerif.svg"
  let
    diagram :: Diagram B
    diagram = paragraph font t <> vrule 100 # moveOriginBy (r2 (0, 50)) # translateX 500
  mainWith diagram
