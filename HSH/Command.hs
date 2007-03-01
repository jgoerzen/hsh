{- Commands for HSH
Copyright (C) 2004-2007 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

{- |
   Module     : HSH.Command
   Copyright  : Copyright (C) 2006-2007 John Goerzen
   License    : GNU LGPL, version 2.1 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Copyright (c) 2006-2007 John Goerzen, jgoerzen\@complete.org
-}

module HSH.Command (ShellCommand(..),
                    PipeCommand(..),
                    (-|-),
                    run,
                    runS,
                    InvokeResult
                   ) where

import System.Cmd.Utils hiding (pipeBoth)
import System.IO.HVIO
import System.IO.Utils
import System.IO
import System.Exit
import System.Posix.Types
import System.Posix.IO
import System.Posix.Process
import System.Log.Logger
import System.IO.Error
import Data.Maybe.Utils
import Data.Maybe
import Data.List.Utils(uniq)
import Control.Exception(evaluate)
import System.Posix.Env

d = debugM "HSH.Command"

{- | Result type for shell commands -}
type InvokeResult = (String, IO ProcessStatus)

{- | A shell command is something we can invoke, pipe to, pipe from,
or pipe in both directions.  All commands that can be run as shell
commands must define these methods. 

Minimum implementation is 'fdInvoke'.
-}
class (Show a) => ShellCommand a where
    {- | Invoke a command. -}
    fdInvoke :: a               -- ^ The command
             -> Fd              -- ^ fd to pass to it as stdin
             -> Fd              -- ^ fd to pass to it as stdout
             -> [Fd]            -- ^ Fds to close in the child.  Will not harm Fds this child needs for stdin and stdout.
             -> (IO ())           -- ^ Action to run post-fork in child (or in main process if it doesn't fork)
             -> IO [InvokeResult]           -- ^ Returns an action that, when evaluated, waits for the process to finish and returns an exit code.

instance Show (String -> String) where
    show _ = "(String -> String)"
instance Show (String -> IO String) where
    show _ = "(String -> IO String)"
  
instance ShellCommand (String -> IO String) where
    fdInvoke func fstdin fstdout childclosefds childfunc =
        do d $ "SIOSF: Before fork"
           p <- try (forkProcess childstuff)
           pid <- case p of
                    Right x -> return x
                    Left x -> fail $ "Error in fork for func: " ++ show x
           d $ "SIOSFP: New func pid " ++ show pid
           return $ seq pid pid
           return [(show func,
                   getProcessStatus True False pid >>=
                                    (return . forceMaybe))]
        where childstuff = do closefds childclosefds [fstdin, fstdout]
                              d $ "SIOSFC Input is on " ++ show fstdin
                              hr <- fdToHandle fstdin
                              d $ "SIOSFC Output is on " ++ show fstdout
                              hw <- fdToHandle fstdout
                              hSetBuffering hw LineBuffering
                              d $ "SIOSFC Running child func"
                              childfunc
                              d $ "SIOSFC Running func in child"
                              contents <- hGetContents hr
                              d $ "SIOSFC Contents read"
                              result <- func contents
                              d $ "SIOSFC Func applied"
                              hPutStr hw result
                              d $ "SIOSFC Func done, closing handles."
                              hClose hr
                              hClose hw
                              d $ "SIOSFC Child exiting."
                              -- It hung here without the exitImmediately
                              --exitImmediately ExitSuccess

{- | An instance of 'ShellCommand' for a pure Haskell function mapping
String to String.  Implement in terms of (String -> IO String) for
simplicity. -}
instance ShellCommand (String -> String) where
    fdInvoke func fstdin fstdout childclosefds childfunc =
        fdInvoke iofunc fstdin fstdout childclosefds childfunc
            where iofunc :: String -> IO String
                  iofunc = return . func

instance Show ([String] -> [String]) where
    show _ = "([String] -> [String])"
instance Show ([String] -> IO [String]) where
    show _ = "([String] -> IO [String])"

{- | An instance of 'ShellCommand' for a pure Haskell function mapping
[String] to [String].

A [String] is generated from a Handle via the 'lines' function, and the
reverse occurs via 'unlines'.

So, this function is intended to operate upon lines of input and produce
lines of output. -}
instance ShellCommand ([String] -> [String]) where
    fdInvoke func = fdInvoke (unlines . func . lines)

{- | The same for an IO function -}
instance ShellCommand ([String] -> IO [String]) where
    fdInvoke func = fdInvoke iofunc
        where iofunc input = do r <- func (lines input)
                                return (unlines r)


{- | An instance of 'ShellCommand' for an external command.  The
first String is the command to run, and the list of Strings represents the
arguments to the program, if any. -}
instance ShellCommand (String, [String]) where
    fdInvoke pc@(cmd, args) fstdin fstdout childclosefds childfunc = 
        do d $ "S Before fork for " ++ show pc
           p <- try (forkProcess childstuff)
           pid <- case p of
                    Right x -> return x
                    Left x -> fail $ "Error in fork: " ++ show x
           d $ "SP New pid " ++ show pid ++ " for " ++ show pc
           return $ seq pid pid
           return [(show (cmd, args), 
                   getProcessStatus True False pid >>=
                                        (return . forceMaybe))]
           
        where 
              childstuff = do d $ "SC preparing to redir"
                              d $ "SC input is on " ++ show fstdin
                              d $ "SC output is on " ++ show fstdout
                              redir fstdin stdInput
                              redir fstdout stdOutput
                              closefds childclosefds [fstdin, fstdout, 0, 1]
                              childfunc
                              d ("Running: " ++ cmd ++ " " ++ (show args))
                              executeFile cmd True args Nothing

{- | An instance of 'ShellCommand' for an external command.  The
String is split using words to the command to run, and the arguments, if any. -}
instance ShellCommand String where
    fdInvoke cmdline ifd ofd closefd forkfunc = 
        do esh <- getEnv "SHELL"
           let sh = case esh of
                      Nothing -> "/bin/sh"
                      Just x -> x
           fdInvoke (sh, ["-c", cmdline]) ifd ofd closefd forkfunc

redir fromfd tofd 
    | fromfd == tofd = do d $ "ignoring identical redir " ++ show fromfd
                          return ()
    | otherwise = do d $ "running dupTo " ++ show (fromfd, tofd)
                     dupTo fromfd tofd
                     closeFd fromfd

closefds :: [Fd]                   -- ^ List of Fds to possibly close
         -> [Fd]                   -- ^ List of Fds to not touch, ever
         -> IO ()
closefds inpclosefds noclosefds =
    do d $ "closefds " ++ show closefds ++ " " ++ show noclosefds
       mapM_ closeit . filter (\x -> not (x `elem` noclosefds)) $ closefds
    where closeit fd = do d $ "Closing fd " ++ show fd
                          closeFd fd
          closefds = uniq inpclosefds

data (ShellCommand a, ShellCommand b) => PipeCommand a b = PipeCommand a b
   deriving Show

{- | An instance of 'ShellCommand' represeting a pipeline. -}
instance (ShellCommand a, ShellCommand b) => ShellCommand (PipeCommand a b) where
    fdInvoke pc@(PipeCommand cmd1 cmd2) fstdin fstdout childclosefds forkfunc = 
        do d $ "*** Handling pipe: " ++ show pc
           (reader, writer) <- createPipe
           let allfdstoclose = reader : writer : fstdin : fstdout : childclosefds
           d $ "pipd fdInvoke: New pipe endpoints: " ++ show (reader, writer)
           res1 <- fdInvoke cmd1 fstdin writer allfdstoclose forkfunc
           res2 <- fdInvoke cmd2 reader fstdout allfdstoclose forkfunc
           d $ "pipe fdInvoke: Parent closing " ++ show [reader, writer]
           mapM_ closeFd [reader, writer]
           
           d $ "*** Done handling pipe " ++ show pc
           return $ res1 ++ res2

{- | Pipe the output of the first command into the input of the second. -}
(-|-) :: (ShellCommand a, ShellCommand b) => a -> b -> PipeCommand a b
(-|-) = PipeCommand 

{- | Function to use when there is nothing for the parent to do -}
nullParentFunc :: IO ()
nullParentFunc = return ()

{- | Function to use when there is nothing for the child to do -}
nullChildFunc :: IO ()
nullChildFunc = return ()

{- | Runs, with input from stdin and output to stdout. -}
run :: ShellCommand a => a -> IO ()
run cmd = 
    do r <- fdInvoke cmd stdInput stdOutput [] nullChildFunc
       checkResults r

{- | Runs, with input from stdin and output to a Haskell string. 

The result string is NOT computed lazily.  Do not use this function
with a large amount of data. -}
runS :: ShellCommand a => a -> IO String
runS cmd =
    do (pread, pwrite) <- createPipe
       d $ "runS: new pipe endpoints: " ++ show [pread, pwrite]
       d "runS 1"
       r <- fdInvoke cmd stdInput pwrite [pread, pwrite] nullChildFunc
       d $ "runS 2 closing " ++ show pwrite
       closeFd pwrite
       d "runS 3"
       hread <- fdToHandle pread
       d "runS 4"
       c <- hGetContents hread
       d "runS 5"
       evaluate (length c)
       d "runS 6"
       hClose hread
       d "runS 7"
       checkResults r
       d "runS 8"
       return c
       
{- | Evaluates result codes and raises an error for any bad ones it finds. -}
checkResults :: [InvokeResult] -> IO ()
checkResults r = 
    do rc <- mapM procresult r
       case catMaybes rc of
         [] -> return ()
         x -> fail (unlines x)
    where procresult :: InvokeResult -> IO (Maybe String)
          procresult (cmd, action) = 
              do d $ "Procresult on: " ++ show cmd
                 rc <- action
                 return $ case rc of
                   Exited (ExitSuccess) -> Nothing
                   Exited (ExitFailure x) -> Just $ cmd ++ ": exited with code " ++ show x
                   Terminated sig -> 
                       Just $ cmd ++ ": Terminated by signal " ++ show sig
                   Stopped sig ->
                       Just $ cmd ++ ": Stopped by signal " ++ show sig

