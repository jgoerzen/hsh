import HSH
import Text.Printf
import System.Log.Logger
import System.Log.Handler.Syslog

main = 
 do s <- openlog "test" [PID] USER DEBUG
    updateGlobalLogger rootLoggerName (addHandler s . setLevel DEBUG)
    runIO $ echo "bar\nbaz\nfoo\nquux\n" -|- "tr a-z A-Z" -|- "grep BA"
    --run $ ("ls", ["-l"]) -|-  countLines -|- ("grep", ["hs$"])
    --run $ (id::(String -> String)) -|- ("wc", ["-l"]) -|- countLines -|- ("grep", ["1"])
        --run $ ("ls", ["-l"]) -|- ("wc", ["-l"])

countLines :: [String] -> [String]
countLines = zipWith (\i line -> printf "%-5d %s" i line) [(1::Int)..]
