{- 
Copyright (C) 2004-2007 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

module TestUtils() where
import HUnit
import HSH

testcmd expected cmd =
    do r <- runS cmd
       expected @=? r

cmdcase name expected cmd = TestLabel name $ TestCase (testcmd expected cmd)
