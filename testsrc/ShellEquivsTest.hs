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

tests = TestList
        [tl "abspath" tabspath,
         tl "basename" tbasename,
         tl "dirname" tdirname,
         tl "catFrom" tcatFrom
{-
         tl "catFromS" tcatFromS,
         tl "catTo" tcatTo,
         tl "cd" tcd,
         tl "grep" tgrep,
         tl "grepV" tgrepV,
         tl "egrep" tegrep,
         tl "egrepV" tegrepV,
         tl "pwd" tpwd,
         tl "readlink" treadlink,
         tl "tee" ttee,
         tl "wcL" wcL -}
        ]
    where tl x y = TestLabel x $ TestList y

