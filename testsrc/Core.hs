{- 
Copyright (C) 2004-2007 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

module Core(tests) where
import HUnit
import HSH
import TestUtils

basics =
    [cmdcase "echo" "hi\n" $ "echo hi"]
       
tests = TestList
        [TestLabel "basics" $ TestList basics]

