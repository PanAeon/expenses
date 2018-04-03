module Charts (renderMonthSummary, openViewer) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
import Data.Hourglass -- Data.Time looks better
import Model
import qualified Data.Map as M
import qualified System.IO.Temp as Tmp
import qualified Data.List as L
import Control.Monad(join, void)
import qualified Data.Set as Set
import System.Process
import Data.Colour.SRGB

values :: [Record] -> [ (String,Double,Bool) ]
values xs = [(c ++ " £" ++ (show $ sum ps), realToFrac (sum ps), True) | (c, ps) <- groupped]
  where
    groupped = M.toList $ M.fromListWith (++) [(c ++ "/" ++ printTags tags, [pr]) | (Record _ (CategoryName c) tags pr descr d) <- xs]
    -- FIXME: wrong tags handling
    printTags tags = join $ L.intersperse "/" (getTag <$> Set.toList tags)
    getTag (Tag x) = x


pitem (s,v,o) = pitem_value .~ v
              $ pitem_label .~ s
              $ pitem_offset .~ (if o then 0.5 else 0)
              $ def

-- FIXME: make style more beautiful, with gradual color pallete, fonts, etc.
renderMonthSummary :: [Record] -> IO FilePath
renderMonthSummary records =
  do
    path <- Tmp.emptySystemTempFile "diagram.svg" -- TODO: maybe add month to filename
    toFile def path $ do
      pie_title .= "Expenses - December 2017 - Total: " ++ show total -- FIXME: again, month...
      pie_title_style . font_name .= "Liberation Sans"
      pie_title_style . font_size .= 32
      pie_title_style . font_weight .= FontWeightNormal
      pie_title_style . font_color .= opaque (darken 0.05 white)
      pie_background . fill_color .= opaque (darken 0.5 white) --black `withOpacity` 0.2
      pie_plot . pie_data .= map pitem xs
      pie_plot . pie_label_style . font_name .= "Liberation Sans"
      pie_plot . pie_label_style . font_size .= 18
      pie_plot . pie_label_style . font_color .= opaque (darken 0.05 white)
      pie_plot . pie_label_line_style . line_width .= 1.5
      pie_plot . pie_label_line_style . line_color .= opaque (darken 0.1 white)
      pie_plot . pie_label_line_style . line_join .= LineJoinRound
      pie_plot . pie_colors .= my_pie_colors
      pie_margin .= 40.0
    pure path
  where
    xs = values records
    total = (sum $ fmap g xs) :: Double
    g (a,b,c) = b
    rgb :: Int -> Int -> Int -> Colour Double
    rgb a b c  = sRGB  (fromIntegral a / 255.0)  (fromIntegral b / 255.0) (fromIntegral c / 255.0)
    my_pie_colors = [ --opaque $ rgb 40 53 147
                      opaque $ rgb 239 154 154
                    , opaque $ rgb 244 143 177
                    , opaque $ rgb 206 147 216
                    , opaque $ rgb 179 157 219
                    , opaque $ rgb 159 168 218
                    , opaque $ rgb 144 202 249
                    , opaque $ rgb 129 212 250
                    -- , opaque $ rgb 57 73 171
                    , opaque $ rgb 63 81 181
                    -- , opaque $ rgb 92 107 192
                    , opaque $ rgb 121 134 203
                    -- , opaque $ rgb 0 105 92
                    , opaque $ rgb 0 137 123
                    , opaque $ rgb 38 166 154]


openViewer :: FilePath -> IO ()
openViewer path = void $ spawnCommand ("xdg-open " ++ show path) -- FIXME: bloody mac

stubData :: [Record]
stubData = [ Record Nothing (CategoryName "fun") (Set.fromList []) 20.0 Nothing (Date 2017 (toEnum 3) 19)
           , Record Nothing (CategoryName "rent") (Set.fromList []) 15.0 Nothing (Date 2017 (toEnum 3) 19)
           , Record Nothing (CategoryName "food") (Set.fromList []) 6.0 Nothing (Date 2017 (toEnum 3) 19)
           , Record Nothing (CategoryName "sport") (Set.fromList []) 14.0 Nothing (Date 2017 (toEnum 3) 19)
           , Record Nothing (CategoryName "stuff") (Set.fromList []) 2.0 Nothing (Date 2017 (toEnum 3) 19)
           ]

renderStub :: IO ()
renderStub =
   do
     p <- renderMonthSummary stubData
     openViewer p
