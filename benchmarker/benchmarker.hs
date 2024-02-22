

import Control.Exception (evaluate)
import qualified Data.Text.IO
import Data.Time (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import Toml (parse)

main :: IO ()
main =
 do args <- getArgs
    filename <- case args of
      [filename] -> pure filename
      _ -> fail "Usage: benchmarker <file.toml>"
    txt <- Data.Text.IO.readFile filename
    start <- getCurrentTime
    evaluate (parse txt)
    stop <- getCurrentTime
    print (stop `diffUTCTime` start)
