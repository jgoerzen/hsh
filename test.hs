import HSH
import Text.Printf

main = 
    --run $ ("ls", ["-l"]) -|-  countLines -|- ("grep", ["hs$"])
    --run $ (id::(String -> String)) -|- ("wc", ["-l"]) -|- countLines -|- ("grep", ["1"])
    run $ ("ls", ["-l"]) -|- ("wc", ["-l"])

countLines :: [String] -> [String]
countLines = zipWith (\i line -> printf "%-5d %s" i line) [(1::Int)..]
