{- 
Copyright (C) 2004-2007 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

module Core(tests) where
import Test.HUnit
import Test.HUnit.Utils
import HSH
import HSH.ShellEquivs
import TestUtils
import Data.Char
import Control.Exception

basics =
    [cmdcase "echo" "hi\n" $ "echo hi",
     cmdcase "echo as args" "there\n" $ ("echo", ["there"]),
     cmdcase "more args 1" "100 testsrc/testdata/quux\n" 
                 "wc -l testsrc/testdata/quux",
     cmdcase "more args 2" "100 testsrc/testdata/quux\n" 
                 ("wc", ["-l", "testsrc/testdata/quux"])
    ]

pipes =
    [cmdcase "sh2sh" "100\n" $ "cat testsrc/testdata/quux" -|- "wc -l",
     cmdcase "sh2sh2sh" "14\n" $
             "cat testsrc/testdata/quux" -|- "grep oo" -|- "wc -l",
     cmdcase "sh2sh2sh2sh" "0000000 032061 000012\n0000003\n" $
             "cat testsrc/testdata/quux" -|- "grep oo" -|- "wc -l" -|- "od",
     cmdcase "ls baseline" lsbase lscmd,
     cmdcase "sh|s->s" (map toUpper lsbase) $ lscmd -|- (map toUpper),
     cmdcase "sh|s->s|sh" "BAR\nBAZ\n" $ 
             lscmd -|- (map toUpper) -|- "grep BA",
     cmdcase "sh|s->s|s->s" "BAR\nBAZ\n" $
             lscmd -|- (map toUpper) -|- grep "BA",
     cmdcase "s->s|sh" (map toUpper lsbase) $ catFromS lsbase -|- "tr a-z A-Z",
     cmdcase "s->s|sh|sh" "BAR\nBAZ\n" $
             catFromS lsbase -|- "tr a-z A-Z" -|- "grep BA",
     cmdcase "s->s|sh|s->s" "BAR\nBAZ\n" $
             catFromS lsbase -|- "tr a-z A-Z" -|- grep "BA",
     cmdcase "s->s|s->s|sh" "BAR\nBAZ\n" $
             catFromS lsbase -|- (map toUpper) -|- "grep BA",
     cmdcase "s->s|s->s|s->s" "BAR\nBAZ\n" $
             catFromS lsbase -|- (map toUpper) -|- grep "BA",
     cmdcase "true" "0\n" $ "true" -|- "wc -l",
     cmdcase "true|true" "" $ "true" -|- "true"
    ]
    where lsbase = "bar\nbaz\nfoo\nquux\n"
          lscmd = "ls testsrc/testdata"

errortests =
    [errcase "ls" "(\"false\",[]): exited with code 1\n"
                 "false",
     errcase "false|true" "(\"false\",[]): exited with code 1\n" $
                  "false" -|- "true",
     errcase "true|false" "(\"false\",[]): exited with code 1\n" $
                  "true" -|- "false",
     errcase "sh80" "(\"sh\",[\"-c\",\"exit 80\"]): exited with code 80\n" $
                  ("sh", ["-c", "exit 80"])
    ]
    where errcase name exp cmd =
              TestLabel name $ TestCase $ 
                        do assertRaises "runS" (IOException (userError exp))
                                        (runS cmd)
                           assertRaises "run" (IOException (userError exp))
                                        (run cmd)
       
tests = TestList
        [TestLabel "basics" $ TestList basics,
         TestLabel "pipes" $ TestList pipes,
         TestLabel "errors" $ TestList errortests]

