{- Commands for HSH
Copyright (C) 2006 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}

{- |
   Module     : HSH.Command
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Copyright (c) 2006 John Goerzen, jgoerzen\@complete.org
-}

module HSH.Command (ShellCommand(..),
                    PipeCmd(..),
                    (-|-)
                   ) where

import MissingH.Cmd hiding (pipeBoth)
import MissingH.IO.HVIO
import MissingH.IO
import Control.Concurrent
import System.IO
import System.Posix.Types
import System.Posix.IO

{- | A shell command is something we can invoke, pipe to, pipe from,
or pipe in both directions.  All commands that can be run as shell
commands must define these methods. 

Any Handles passed in should be assumed to be closed by the functions in here.

Minimum implementation is 'fdInvoke'.

ALL THESE ARE GOING TO NEED TWO NEW ITEMS: one for parent and one for child...

but what about when we don't fork?

for the ones that don't fork, the items for 
 -}
class (Show a) => ShellCommand a where
{-
    {- | Invoke a command, specifying stdin & stdout handles -}
    hInvoke :: a -> Handle -> Handle -> IO ()
-}

    {- | Invoke a command. -}
    fdInvoke :: a               -- ^ The command
             -> Fd              -- ^ fd to pass to it as stdin
             -> Fd              -- ^ fd to pass to it as stdout
             -> (ProcessID -> IO ()) -- ^ If this invocation forks, action to run post-fork in parent.  Ignored if invocation doesn't fork.
             -> IO ()           -- ^ If this invocation forks, action to run post-fork in client.  Ignored if invocation doesn't fork.
             -> IO ()

{-
    hInvoke cmd h0 h1 = 
        do fd0 <- handleToFd h0
           fd1 <- handleToF2 h1
           fdInvoke cmd fd0 fd1
-}

instance Show ([Char] -> [Char]) where
    show _ = "(String -> String)"
  
{- | An instance of 'ShellCommand' for a pure Haskell function mapping
String to String. -}
instance ShellCommand ([Char] -> [Char]) where
    fdInvoke func fstdin fstdout _ _ =
        do putStrLn "HI!"
           putStrLn (show fstdin)
           hreader <- fdToHandle fstdin
           hwriter <- fdToHandle fstdout
           incontents <- hGetContents hreader
           putStrLn "Before forkIO"
           forkOS $ do
                       putStrLn "After forkIO"
                       putStrLn incontents
                       putStrLn (show incontents)
                       hPutStr hwriter (func "foo")
                       --hClose hwriter
                       threadDelay 50000
           threadDelay 10000000
           return ()

instance Show ([[Char]] -> [[Char]]) where
    show _ = "([String] -> [String])"

{- | An instance of 'ShellCommand' for a pure Haskell function mapping
[String] to [String].

A [String] is generated from a Handle via the 'lines' function, and the
reverse occurs via 'unlines'.

So, this function is intended to operate upon lines of input and produce
lines of output. -}

instance ShellCommand ([[Char]] -> [[Char]]) where
    fdInvoke func = fdInvoke (unlines . func . lines)


{- | An instance of 'ShellCommand' for an external command.  The
first String is the command to run, and the list of Strings represents the
arguments to the program, if any. -}
instance ShellCommand ([Char], [[Char]]) where
    fdInvoke (cmd, args) fstdin fstdout parentfunc childfunc = 
        pOpen3 (Just fstdin) (Just fstdout) Nothing
               cmd args parentfunc childfunc

data (ShellCommand a, ShellCommand b) => PipeCmd a b = PipeCmd a b
   deriving Show

{- | An instance of 'ShellCommand' represeting a pipeline. -}
instance (ShellCommand a, ShellCommand b) => ShellCommand (PipeCmd a b) where
    fdInvoke (PipeCmd cmd1 cmd2) fstdin fstdout parentfunc childfunc = 
        do (reader, writer) <- createPipe
           fdInvoke cmd1 fstdin writer 
                        (\pid -> parentfunc pid >> closeFd writer)
                        (childfunc >> closeFd reader)
           fdInvoke cmd2 reader fstdout 
                        (\pid -> parentfunc pid >> closeFd reader)
                        (childfunc >> closeFd writer)

{- | Pipe the output of the first command into the input of the second. -}
(-|-) :: (ShellCommand a, ShellCommand b) => a -> b -> PipeCmd a b
(-|-) = PipeCmd 

