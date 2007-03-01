{- 
Copyright (C) 2004-2007 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

module Tests(tests) where
import Test.HUnit
import qualified Testbasics
import qualified TestSbasics
import qualified SpecificDBTests
import qualified TestMisc

test1 = TestCase ("x" @=? "x")

tests = TestList [TestLabel "test1" test1,
                  TestLabel "core" Core.tests]

                  TestLabel "String basics" TestSbasics.tests,
                  TestLabel "SqlValue basics" Testbasics.tests,
                  TestLabel "SpecificDB" SpecificDBTests.tests,
                  TestLabel "Misc tests" TestMisc.tests]
