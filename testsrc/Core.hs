{- 
Copyright (C) 2004-2007 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

module Core(tests) where
import Test.HUnit
import Test.HUnit.Tools
import HSH
import HSH.ShellEquivs
import TestUtils
import Data.Char
import Control.Exception
import qualified Data.ByteString.Lazy as BSL

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
     cmdcase "sh2sh2sh2sh" "0000000 061 064 012\n0000003\n" $
             "cat testsrc/testdata/quux" -|- "grep oo" -|- "wc -l" -|- "od -b",
     cmdcase "ls baseline" lsbase lscmd,
     cmdcase "sh|s->s" (map toUpper lsbase) $ lscmd -|- (map toUpper),
     cmdcase "sh|s->s|sh" "BAR\nBAZ\n" $ 
             lscmd -|- (map toUpper) -|- "grep BA",
     cmdcase "sh|s->s|s->s" "BAR\nBAZ\n" $
             lscmd -|- (map toUpper) -|- grep "BA",
     cmdcase "s->s|sh" (map toUpper lsbase) $ echo lsbase -|- "tr a-z A-Z",
     cmdcase "s->s|sh|sh" "BAR\nBAZ\n" $
             echo lsbase -|- "tr a-z A-Z" -|- "grep BA",
     cmdcase "s->s|sh|sh BS" (str2bs "BAR\nBAZ\n") $
             echo lsbase -|- "tr a-z A-Z" -|- "grep BA",
     cmdcase "BS s->s|sh|sh" "BAR\nBAZ\n" $
             echo lsbaseBSL -|- "tr a-z A-Z" -|- "grep BA",
     cmdcase "BS s->s|sh|sh BS" (str2bs "BAR\nBAZ\n") $
             echo lsbaseBSL -|- "tr a-z A-Z" -|- "grep BA",
     cmdcase "s->s|sh|s->s" "BAR\nBAZ\n" $
             echo lsbase -|- "tr a-z A-Z" -|- grep "BA",
     cmdcase "BS s->s|sh|s->s" "BAR\nBAZ\n" $
             echo lsbaseBSL -|- "tr a-z A-Z" -|- grep "BA",
     cmdcase "s->s|s->s|sh" "BAR\nBAZ\n" $
             echo lsbase -|- (map toUpper) -|- "grep BA",
     cmdcase "BS s->s|s->s|sh" "BAR\nBAZ\n" $
             echo lsbaseBSL -|- (map toUpper) -|- "grep BA",
     cmdcase "s->s|s->s|s->s" "BAR\nBAZ\n" $
             echo lsbase -|- (map toUpper) -|- grep "BA",
     cmdcase "BS s->s|s->s|s->s" "BAR\nBAZ\n" $
             echo lsbaseBSL -|- (map toUpper) -|- grep "BA",
     cmdcase "true" "0\n" $ "true" -|- "wc -l",
     cmdcase "true|true" "" $ "true" -|- "true",
     cmdcase "shell" "testsrc/testdata/bar\ntestsrc/testdata/baz\ntestsrc/testdata/foo\ntestsrc/testdata/quux\n" "ls testsrc/testdata/*"
    ]
    where lsbase = "bar\nbaz\nfoo\nquux\n"
          lsbaseBSL = str2bs lsbase
          lscmd = "ls testsrc/testdata"
          str2bs = BSL.pack . map (fromIntegral . fromEnum)

sleeptests = 
    [cmdcase "sleep 0.10" "" "sleep 0.10",
     cmdcase "sleep|true" "" $ "sleep 0.10" -|- "true",
     cmdcase "true|sleep" "" $ "true" -|- "sleep 0.10",
     cmdcase "true|sleep|true" "" $ "true" -|- "sleep 0.10" -|- "true"
    ]
    
errortests =
    [errcase "ls" "(\"false\",[]): exited with code 1"
                 ("false", []::[String]),
     errcase "false|true" "(\"false\",[]): exited with code 1" $
                  ("false", []::[String]) -|- "true",
     errcase "true|false" "(\"false\",[]): exited with code 1" $
                  "true" -|- ("false", []::[String]),
     errcase "sh80" "(\"sh\",[\"-c\",\"exit 80\"]): exited with code 80" $
                  ("sh", ["-c", "exit 80"]),
     cmdcase "bfalse" False "false",
     cmdcase "btrue" True "true",
     cmdcase "b80" False ("sh", ["-c", "exit 80"]),
     cmdcase "i80" (80::Int) ("sh", ["-c", "exit 80"]),
     cmdcase "i0" (0::Int) "true",
     cmdcase "i1" (1::Int) "false"
    ]
    where errcase name exp cmd =
              TestLabel name $ TestCase $ 
                        do assertRaises "runS" (userError exp)
                                        ((run cmd)::IO String)
                           assertRaises "run"  (userError exp)
                                        ((run cmd)::IO ())
       
utf8tests = 
  let teststr = "привет" in
  [cmdcase "basic utf8" teststr ("echo " ++ teststr)
  ]

tests = TestList
        [TestLabel "basics" $ TestList basics,
         TestLabel "pipes" $ TestList pipes,
         TestLabel "errors" $ TestList errortests,
         TestLabel "sleep" $ TestList sleeptests,
         TestLabel "utf8" $ TestList utf8tests]

