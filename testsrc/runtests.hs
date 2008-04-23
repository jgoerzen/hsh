{- 
Copyright (C) 2004-2007 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

module Main where 

import Test.HUnit
import Tests
import TestUtils
import System.IO
import Text.Printf

-- main = do runTestTT tests
main = do (c, _) <- performTest reportStart reportError reportFailure () tests
          printf "\n DONE: %d\n" (tried c)

reportStart :: ReportStart ()
reportStart st () =
    do putStrLn $ "reportStart : " ++ (showPath . path $ st)
       printf "[%-4d] START   %s\n" (tried . counts $ st)
               (showPath . path $ st)
       return ()

reportError :: ReportProblem ()
reportError = problem "ERROR  "

reportFailure :: ReportProblem ()
reportFailure = problem "FAILURE"

problem :: String -> ReportProblem ()
problem ptype ptext st () =
    do putStrLn "problem"
       printf "[%-4d] %s %s\n       %s\n" (tried . counts $ st)
           (showPath . path $ st) ptext
       return ()

