{- 
Copyright (C) 2004-2007 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

module Core(tests) where
import Test.HUnit
import HSH
import TestUtils

basics =
    [cmdcase "echo" "hi\n" $ "echo hi",
     cmdcase "echo as args" "there\n" $ ("echo", ["there"]),
     cmdcase "more args 1" "100 testsrc/testdata/quux\n" 
                 "wc -l testsrc/testdata/quux",
     cmdcase "more args 2" "100 testsrc/testdata/quux\n" 
                 ("wc", ["-l", "testsrc/testdata/quux"])
    ]

basicpipe =
    [cmdcase "sh2sh" "100\n" $ "cat testsrc/testdata/quux" -|- "wc -l",
     cmdcase "sh2sh2sh" "14\n" $
             "cat testsrc/testdata/quux" -|- "grep oo" -|- "wc -l",
     cmdcase "sh2sh2sh2sh" "0000000 032061 000012\n0000003\n" $
             "cat testsrc/testdata/quux" -|- "grep oo" -|- "wc -l" -|- "od"
    ]
       
tests = TestList
        [TestLabel "basics" $ TestList basics,
         TestLabel "basicpipe" $ TestList basicpipe]

