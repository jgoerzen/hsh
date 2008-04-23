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
main = do hSetBuffering stdout LineBuffering
          hSetBuffering stderr LineBuffering
          (c, _) <- performTest reportStart reportError reportFailure () tests
          printf "\n DONE: %d\n" (tried c)

reportStart :: ReportStart ()
reportStart st () =
    do printf "[%-4d] START   %s\n" (tried . counts $ st)
               (showPath . path $ st)
       hFlush stdout
       return ()

reportError :: ReportProblem ()
reportError = problem "ERROR  "

reportFailure :: ReportProblem ()
reportFailure = problem "FAILURE"

problem :: String -> ReportProblem ()
problem ptype ptext st () =
    do printf "[%-4d] %s %s\n       %s\n" (tried . counts $ st)
           (showPath . path $ st) ptext
       hFlush stdout
       return ()

