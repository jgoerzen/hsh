{- 
Copyright (C) 2004-2007 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

module Tests(tests) where
import Test.HUnit
import qualified Core
import qualified ShellEquivsTest

test1 = TestCase ("x" @=? "x")

tests = TestList [TestLabel "test1" test1,
                  TestLabel "core" Core.tests,
                  TestLabel "ShellEquivs" ShellEquivsTest.tests]
