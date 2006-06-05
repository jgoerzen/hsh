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
                    PipeCommand(..),
                    (-|-),
                    run,
                    InvokeResult
                   ) where

import MissingH.Cmd hiding (pipeBoth)
import MissingH.IO.HVIO
import MissingH.IO
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Exit
import System.Posix.Types
import System.Posix.IO
import System.Posix.Process
import MissingH.Logging.Logger
import System.IO.Error
import MissingH.Maybe
import Data.Maybe

{- | Result type for shell commands -}
type InvokeResult = (String, IO ProcessStatus)

{- | Type for functions. -}
data InvokeType = Forking | NonForking | Pipe
                deriving (Eq, Show)

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
    {- | Invoke a command. -}
    fdInvoke :: a               -- ^ The command
             -> Fd              -- ^ fd to pass to it as stdin
             -> Fd              -- ^ fd to pass to it as stdout
             -> (ProcessID -> InvokeType -> IO ()) -- ^ Action to run post-fork in parent (or in main process if it doesn't fork; PID is 0 in these cases)
             -> (InvokeType -> IO ())           -- ^ Action to run post-fork in child (or in main process if it doesn't fork)
             -> IO [InvokeResult]           -- ^ Returns an action that, when evaluated, waits for the process to finish and returns an exit code.

instance Show ([Char] -> [Char]) where
    show _ = "(String -> String)"
  
{- | An instance of 'ShellCommand' for a pure Haskell function mapping
String to String. -}
instance ShellCommand ([Char] -> [Char]) where
    fdInvoke func fstdin fstdout parentfunc childfunc =
        do hreader <- fdToHandle fstdin
           hwriter <- fdToHandle fstdout
           incontents <- hGetContents hreader
           mv <- newEmptyMVar
           parentfunc 0 NonForking
           forkIO $ do childfunc 0 NonForking
                       hPutStr hwriter (func incontents)
                       hClose hwriter
                       putMVar mv (Exited ExitSuccess)
           return [(show func, takeMVar mv)]

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
        do debugM "HSH.Command.shell.fdInvoke" "Before fork"
           p <- try (forkProcess childstuff)
           pid <- case p of
                    Right x -> return x
                    Left x -> fail $ "Error in fork: " ++ show x
           retval <- parentfunc $! pid $! Forking
           return $ seq retval retval
           return [(show (cmd, args), 
                   (getProcessStatus True False pid >>=
                                        (return . forceMaybe)))]
           
        where redir fromfd tofd 
                  | fromfd == tofd = return ()
                  | otherwise = do dupTo fromfd tofd
                                   closeFd fromfd
              childstuff = do redir fstdin stdInput
                              redir fstdout stdOutput
                              childfunc Forking
                              debugM "HSH.Command.shell.fdInvoke" 
                                     ("Running: " ++ cmd ++ " " ++
                                      (show args))
                              executeFile cmd True args Nothing

data (ShellCommand a, ShellCommand b) => PipeCommand a b = PipeCommand a b
   deriving Show

{- | An instance of 'ShellCommand' represeting a pipeline. -}
instance (ShellCommand a, ShellCommand b) => ShellCommand (PipeCommand a b) where
    fdInvoke (PipeCommand cmd1 cmd2) fstdin fstdout parentfunc childfunc = 
        do (reader, writer) <- createPipe
           res1 <- fdInvoke cmd1 fstdin writer 
                        (\pid -> parentfunc pid)
                        (childfunc Pipe >> closeFd reader)
           res2 <- fdInvoke cmd2 reader fstdout 
                        (\pid -> parentfunc pid >> closeFd reader >> closeFd writer )
                        (childfunc Pipe >> closeFd writer)
           parentfunc Pipe
           closeFd reader
           closeFd writer
           return $ res1 ++ res2
        where res1parent pid Forking = closeFd writer
              res1parent pid Pipe = mapM_ closeFd [reader, writer]
              res1parent pid _ = return ()
              res1client 
              

{- | Pipe the output of the first command into the input of the second. -}
(-|-) :: (ShellCommand a, ShellCommand b) => a -> b -> PipeCommand a b
(-|-) = PipeCommand 

{- | Function to use when there is nothing for the parent to do -}
nullParentFunc :: ProcessID -> IO ()
nullParentFunc = (\_ -> return ())

{- | Function to use when there is nothing for the child to do -}
nullChildFunc :: IO ()
nullChildFunc = return ()

{- | Runs, with input from stdin and output to stdout. -}
run :: ShellCommand a => a -> IO ()
run cmd = 
    do r <- fdInvoke cmd stdInput stdOutput  nullParentFunc nullChildFunc
       checkResults r
       
{- | Evaluates result codes and raises an error for any bad ones it finds. -}
checkResults :: [InvokeResult] -> IO ()
checkResults r = 
    do rc <- mapM procresult r
       case catMaybes rc of
         [] -> return ()
         x -> fail (unlines x)
    where procresult :: InvokeResult -> IO (Maybe String)
          procresult (cmd, action) = 
              do rc <- action
                 return $ case rc of
                   Exited (ExitSuccess) -> Nothing
                   Exited (ExitFailure x) -> Just $ cmd ++ ": exited with code " ++ show x
                   Terminated sig -> 
                       Just $ cmd ++ ": Terminated by signal " ++ show sig
                   Stopped sig ->
                       Just $ cmd ++ ": Stopped by signal " ++ show sig
