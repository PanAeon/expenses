module Charts () where

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


values :: [Record] -> [ (String,Double,Bool) ]
values xs = [(c ++ " £" ++ (show $ sum ps), realToFrac (sum ps), False) | (c, ps) <- groupped]
  where
    groupped = M.toList $ M.fromListWith (++) [(c ++ "/" ++ printTags tags, [pr]) | (Record _ (CategoryName c) tags pr descr d) <- xs]
    -- FIXME: wrong tags handling
    printTags tags = join $ L.intersperse "/" (getTag <$> Set.toList tags)
    getTag (Tag x) = x


pitem (s,v,o) = pitem_value .~ v
              $ pitem_label .~ s
              $ pitem_offset .~ (if o then 25 else 0)
              $ def

renderMonthSummary :: [Record] -> IO FilePath
renderMonthSummary records =
  do
    path <- Tmp.emptySystemTempFile "diagram.svg" -- TODO: maybe add month to filename
    toFile def path $ do
      pie_title .= "Expenses - December 2017 - Total: " ++ show total -- FIXME: again, month...
      pie_plot . pie_data .= map pitem xs
    pure path
  where
    xs = values records
    total = (sum $ fmap g xs) :: Double
    g (a,b,c) = b

openViewer :: FilePath -> IO ()
openViewer path = void $ spawnCommand ("xdg-open " ++ show path) -- FIXME: bloody mac 
