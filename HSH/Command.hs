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
                    RunResult,
                    run,
                    InvokeResult,
                    tryEC,
                    catchEC,
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
import Text.Regex.Posix

d = debugM "HSH.Command"
dr = debugM "HSH.Command.Run"

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
        do -- d $ "SIOSF: Before fork"
           p <- try (forkProcess childstuff)
           pid <- case p of
                    Right x -> return x
                    Left x -> fail $ "Error in fork for func: " ++ show x
           -- d $ "SIOSFP: New func pid " ++ show pid
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
                              dr ("RUN: " ++ cmd ++ " " ++ (show args))
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

{- | Different ways to get data from 'run'. 

 * IO () runs, throws an exception on error, and sends stdout to stdout 

 * IO String runs, throws an exception on error, reads stdout into
   a buffer, and returns it as a string.

 * IO [String] is same as IO String, but returns the results as lines

 * IO ProcessStatus runs and returns a ProcessStatus with the exit
   information.  stdout is sent to stdout.  Exceptions are not thrown. 

 * IO (String, ProcessStatus) is like IO ProcessStatus, but also
   includes a description of the last command in the pipe to have
   an error (or the last command, if there was no error)

 * IO Int returns the exit code from a program directly.  If a signal
   caused the command to be reaped, returns 128 + SIGNUM.

 * IO Bool returns True if the program exited normally (exit code 0, 
   not stopped by a signal) and False otherwise.

-}
class RunResult a where
    run :: (ShellCommand b) => b -> a

instance RunResult (IO ()) where
    run cmd = run cmd >>= checkResults

instance RunResult (IO (String, ProcessStatus)) where
    run cmd =
        do r <- fdInvoke cmd stdInput stdOutput [] nullChildFunc
           processResults r

instance RunResult (IO ProcessStatus) where
    run cmd = ((run cmd)::IO (String, ProcessStatus)) >>= return . snd

instance RunResult (IO Int) where
    run cmd = do rc <- run cmd
                 case rc of
                   Exited (ExitSuccess) -> return 0
                   Exited (ExitFailure x) -> return x
                   Terminated x -> return (128 + (fromIntegral x))
                   Stopped x -> return (128 + (fromIntegral x))
                 
instance RunResult (IO Bool) where
    run cmd = do rc <- run cmd
                 return ((rc::Int) == 0)

instance RunResult (IO [String]) where
    run cmd = do r <- run cmd
                 return (lines r)

instance RunResult (IO String) where
    run cmd =
        do (pread, pwrite) <- createPipe
           -- d $ "runS: new pipe endpoints: " ++ show [pread, pwrite]
           -- d "runS 1"
           r <- fdInvoke cmd stdInput pwrite [pread, pwrite] nullChildFunc
           -- d $ "runS 2 closing " ++ show pwrite
           closeFd pwrite
           -- d "runS 3"
           hread <- fdToHandle pread
           -- d "runS 4"
           c <- hGetContents hread
           -- d "runS 5"
           evaluate (length c)
           -- d "runS 6"
           hClose hread
           -- d "runS 7"
           processResults r >>= checkResults
           -- d "runS 8"
           return c

{- | Evaluates the result codes and returns an overall status -}
processResults :: [InvokeResult] -> IO (String, ProcessStatus)
processResults r = 
    do rc <- mapM procresult r
       case catMaybes rc of
         [] -> return (fst (last r), Exited (ExitSuccess))
         x -> return (last x)
    where procresult :: InvokeResult -> IO (Maybe (String, ProcessStatus))
          procresult (cmd, action) = 
              do rc <- action
                 return $ case rc of
                   Exited (ExitSuccess) -> Nothing
                   x -> Just (cmd, x)

{- | Evaluates result codes and raises an error for any bad ones it finds. -}
checkResults :: (String, ProcessStatus) -> IO ()
checkResults (cmd, ps) =
       case ps of
         Exited (ExitSuccess) -> return ()
         Exited (ExitFailure x) -> 
             fail $ cmd ++ ": exited with code " ++ show x
         Terminated sig -> 
             fail $ cmd ++ ": terminated by signal " ++ show sig
         Stopped sig ->
             fail $ cmd ++ ": stopped by signal " ++ show sig
       
{- | Handle an exception derived from a program exiting abnormally -}
tryEC :: IO a -> IO (Either ProcessStatus a)
tryEC action =
    do r <- try action
       case r of
         Left ioe ->
          if isUserError ioe then
              case (ioeGetErrorString ioe =~~ pat) of
                Nothing -> ioError ioe -- not ours; re-raise it
                Just e -> return . Left . proc $ e
          else ioError ioe      -- not ours; re-raise it
         Right result -> return (Right result)
    where pat = ": exited with code [0-9]+$|: terminated by signal ([0-9]+)$|: stopped by signal [0-9]+"
          proc :: String -> ProcessStatus
          proc e 
              | e =~ "^: exited" = Exited (ExitFailure (str2ec e))
              | e =~ "^: terminated by signal" = Terminated (str2ec e)
              | e =~ "^: stopped by signal" = Stopped (str2ec e)
              | otherwise = error "Internal error in tryEC"
          str2ec e =
              read (e =~ "[0-9]+$")

{- | Catch an exception derived from a program exiting abnormally -}
catchEC :: IO a -> (ProcessStatus -> IO a) -> IO a
catchEC action handler =
    do r <- tryEC action
       case r of
         Left ec -> handler ec
         Right result -> return result
