

import Control.Exception (evaluate)
import Data.Time (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import Toml (parse)

main :: IO ()
main =
 do args <- getArgs
    filename <- case args of
      [filename] -> pure filename
      _ -> fail "Usage: benchmarker <file.toml>"
    txt <- readFile filename
    evaluate (length txt) -- readFile uses lazy IO, force it to load
    start <- getCurrentTime
    evaluate (parse txt)
    stop <- getCurrentTime
    print (stop `diffUTCTime` start)
