{- 
Copyright (C) 2004-2007 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

module ShellEquivsTest(tests) where
import Test.HUnit
import Test.HUnit.Utils
import HSH
import HSH.ShellEquivs
import TestUtils
import Data.Char
import Control.Exception
import Data.List

tabspath = []                   -- FIXME: need some way to test this

tbasenamedirname t = 
    [t "" "." ".",
     t "/" "/" "/",
     t "/foo" "foo" "/",
     t "/foo/bar" "bar" "/foo",
     t "/foo/" "foo" "/",
     t "./" "." ".",
     t "/foo/bar/" "bar" "/foo",
     t "/foo/bar/." "." "/foo/bar",
     t "/foo/bar/./" "." "/foo/bar",
     t "foo" "foo" ".",
     t "/usr/lib" "lib" "/usr",
     t "/usr/" "usr" "/",
     t "usr" "usr" ".",
     t "." "." ".",
     t ".." ".." "."
    ]

tbasename = 
    tbasenamedirname 
    (\inp expbn expdn -> TestLabel inp $ TestCase $ assertEqual inp expbn (basename inp))

tdirname = 
    tbasenamedirname 
    (\inp expbn expdn -> TestLabel inp $ TestCase $ assertEqual inp expdn (dirname inp))

tcatFrom = 
    [cmdcase "basic" foo $ catFrom [fn],
     cmdcase "twice" (foo ++ foo) $ catFrom [fn, fn],
     cmdcase "-,foo" ("hi\n" ++ foo) $ "echo hi" -|- catFrom ["-", fn],
     cmdcase "foo,-" (foo ++ "hi\n") $ "echo hi" -|- catFrom [fn, "-"],
     cmdcase "foo,-,cat" "     1\t1234\n     2\t5678\n     3\t14\n     4\thi\n"
             $ "echo hi" -|- catFrom [fn, "-"] -|- "cat -n"
    ]
    where fn = "testsrc/testdata/foo"
          foo = "1234\n5678\n14\n"

tcatFromS =
    [cmdcase "basic" s $ catFromS s,
     cmdcase "shcat" s $ catFromS s -|- "cat",
     cmdcase "cat" s $ catFromS s -|- catFrom ["-"]
    ]
    where s = "testfoobarbaz"

tcatTo = [] -- FIXME: write

tcd =
    [TestCase $ 
     do p <- pwd
        cd "testsrc"
        p2 <- pwd
        assertEqual "after cd" (p ++ "/testsrc") p2
        cd ".."
        p3 <- pwd
        assertEqual "after cd .." p p3
    ]

tgreps =
    [tc "oo" (isInfixOf "oo") (grep "oo") (grepV "oo"),
     tc "nonexistant" (\_ -> False) (grep "nonexistant") 
            (grepV "nonexistant"),
     tc "e ^oo" (isPrefixOf "oo") (egrep "^oo") (egrepV "^oo"),
     tc "e nonexistant" (\_ -> False) (egrep "nonexistant") (egrepV "nonexistant")
    ]
    where tc name filtfunc cmd cmdv = TestLabel name $ TestCase $
              do c <- readFile "testsrc/testdata/quux"
                 let exp = unlines . filter filtfunc . lines $ c
                 let expv = unlines . filter (not . filtfunc) . lines $ c
                 r <- runS (catFrom ["testsrc/testdata/quux"] -|- cmd)
                 assertEqual "grep" exp r
                 r2 <- runS (catFrom ["testsrc/testdata/quux"] -|- cmdv)
                 assertEqual "grepv" expv r2
                 
twcL = 
    [t "null" 0 ("echo", ["-n", ""]),
     t "empty" 1 ("echo", [""]),
     t "no eol" 1 ("echo", ["-n", "foo"]), -- shell does 0 here; which is right?
     t "normal" 1 "echo foo",
     t "quux" 100 (catFrom ["testsrc/testdata/quux"])]
    where t name expint cmd =
              cmdcase name ((show expint) ++ "\n") (cmd -|- wcL)

tests = TestList
        [tl "abspath" tabspath,
         tl "basename" tbasename,
         tl "dirname" tdirname,
         tl "catFrom" tcatFrom,
         tl "catFromS" tcatFromS,
         tl "catTo" tcatTo,
         tl "cd" tcd,
         tl "grep family" tgreps,
         tl "wcL" twcL
{-
         -- tl "pwd" tpwd, -- covered by tcd
         tl "readlink" treadlink,
         tl "tee" ttee -}
        ]
    where tl x y = TestLabel x $ TestList y

