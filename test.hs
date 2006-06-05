import HSH.Command
import System.IO
import System.Posix.IO
import MissingH.Logging.Logger
import MissingH.Logging.Handler.Syslog

{-
main2 = do fdInvoke ("ls", []::[[Char]]) stdInput stdOutput (\_ -> return ()) (return ())
          putStrLn "Done."
          -}

main = 
    do s <- openlog "SyslogStuff" [PID] USER DEBUG
       --updateGlobalLogger rootLoggerName (addHandler s . setLevel DEBUG)
       debugM "MissingH.Cmd" "Test"
       run $ ("ls", ["-l"]) -|-  (zipWith (\i l -> show i ++ ": " ++ l) [1..])
       -- fdInvoke ("ls", []::[[Char]]) stdInput stdOutput (\_ -> return ()) (return ())

