import HSH
import HSH.ShellEquivs
import System.Log.Logger

main = do -- updateGlobalLogger rootLoggerName (setLevel DEBUG)
          putStrLn " *******  RUN 1"
          r <- runS ("ls -l" -|- "grep i" -|- wcL)
          putStrLn $ " *******  RUN 1 result: " ++ show r
          putStrLn " *******  RUN 2"
          r2 <- runS ("ls -l" -|- grep "i" -|- wcL)
          putStrLn $ " *******  RUN 2 result: " ++ show r2

