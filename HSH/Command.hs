{-# OPTIONS_GHC -XFlexibleInstances -XTypeSynonymInstances #-}

{- Commands for HSH
Copyright (C) 2004-2008 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

{- |
   Module     : HSH.Command
   Copyright  : Copyright (C) 2006-2008 John Goerzen
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
                    runIO,
                    runSL,
                    InvokeResult,
                    checkResults,
                    tryEC,
                    catchEC,
                   ) where

-- import System.IO.HVIO
-- import System.IO.Utils
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
import Control.Monad(when)
import Data.String.Utils(rstrip)
import Control.Concurrent
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

d, dr :: String -> IO ()
d = debugM "HSH.Command"
dr = debugM "HSH.Command.Run"

{- | Result type for shell commands.  The String is the text description of
the command, not its output. -}
type InvokeResult = (String, IO ProcessStatus)

{- | A shell command is something we can invoke, pipe to, pipe from,
or pipe in both directions.  All commands that can be run as shell
commands must define these methods.

Minimum implementation is 'fdInvoke'.

Some pre-defined instances include:

 * A simple bare string, which is passed to the shell for execution. The shell
   will then typically expand wildcards, parse parameters, etc.

 * A @(String, [String])@ tuple.  The first item in the tuple gives
   the name of a program to run, and the second gives its arguments.
   The shell is never involved.  This is ideal for passing filenames,
   since there is no security risk involving special shell characters.

 * A @Handle -> Handle -> IO ()@ function, which reads from the first
   handle and write to the second.

 * Various functions.  These functions will accept input representing
   its standard input and output will go to standard output.  

Some pre-defined instance functions include:

 * @(String -> String)@, @(String -> IO String)@, plus the same definitions
   for ByteStrings.

 * @([String] -> [String])@, @([String] -> IO [String])@, where each @String@
   in the list represents a single line

 * @(() -> String)@, @(() -> IO String)@, for commands that explicitly
   read no input.  Useful with closures.  Useful when you want to avoid
   reading stdin because something else already is.  These have the unit as
   part of the function because otherwise we would have conflicts with things
   such as bare Strings, which represent a command name.

-}
class (Show a) => ShellCommand a where
    {- | Invoke a command. -}
    fdInvoke :: a               -- ^ The command
             -> Fd              -- ^ fd to pass to it as stdin
             -> Fd              -- ^ fd to pass to it as stdout
             -> [Fd]            -- ^ Fds to close in the child.  Will not harm Fds this child needs for stdin and stdout.
             -> (IO ())           -- ^ Action to run post-fork in child (or in main process if it doesn't fork)
             -> IO [InvokeResult]           -- ^ Returns an action that, when evaluated, waits for the process to finish and returns an exit code.

instance Show (Handle -> Handle -> IO ()) where
    show _ = "(Handle -> Handle -> IO ())"
instance Show (String -> String) where
    show _ = "(String -> String)"
instance Show (() -> String) where
    show _ = "(() -> String)"
instance Show (String -> IO String) where
    show _ = "(String -> IO String)"
instance Show (() -> IO String) where
    show _ = "(() -> IO String)"
instance Show (BSL.ByteString -> BSL.ByteString) where
    show _ = "(Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString)"
instance Show (() -> BSL.ByteString) where
    show _ = "(() -> Data.ByteString.Lazy.ByteString)"
instance Show (BSL.ByteString -> IO BSL.ByteString) where
    show _ = "(Data.ByteString.Lazy.ByteString -> IO Data.ByteString.Lazy.ByteString)"
instance Show (() -> IO BSL.ByteString) where
    show _ =  "(() -> IO BSL.ByteString)"
instance Show (BS.ByteString -> BS.ByteString) where
    show _ = "(Data.ByteString.ByteString -> Data.ByteString.ByteString)"
instance Show (() -> BS.ByteString) where
    show _ = "(() -> Data.ByteString.ByteString)"
instance Show (BS.ByteString -> IO BS.ByteString) where
    show _ = "(Data.ByteString.ByteString -> IO Data.ByteString.ByteString)"
instance Show (() -> IO BS.ByteString) where
    show _ = "(() -> IO Data.ByteString.ByteString)"

instance ShellCommand (String -> IO String) where
    fdInvoke = genericStringlikeIO hGetContents hPutStr

instance ShellCommand (() -> IO String) where
    fdInvoke = genericStringlikeO hPutStr

instance ShellCommand (BSL.ByteString -> IO BSL.ByteString) where
    fdInvoke = genericStringlikeIO BSL.hGetContents BSL.hPut

instance ShellCommand (() -> IO BSL.ByteString) where
    fdInvoke = genericStringlikeO BSL.hPut

instance ShellCommand (BS.ByteString -> IO BS.ByteString) where
    fdInvoke = genericStringlikeIO BS.hGetContents BS.hPut

instance ShellCommand (() -> IO BS.ByteString) where
    fdInvoke = genericStringlikeO BS.hPut

{- | An instance of 'ShellCommand' for a pure Haskell function mapping
String to String.  Implement in terms of (String -> IO String) for
simplicity. -}
instance ShellCommand (String -> String) where
    fdInvoke func =
        fdInvoke iofunc
            where iofunc :: String -> IO String
                  iofunc = return . func

instance ShellCommand (() -> String) where
    fdInvoke func =
        fdInvoke iofunc
            where iofunc :: () -> IO String
                  iofunc = return . func

instance ShellCommand (BSL.ByteString -> BSL.ByteString) where
    fdInvoke func =
        fdInvoke iofunc
            where iofunc :: BSL.ByteString -> IO BSL.ByteString
                  iofunc = return . func

instance ShellCommand (() -> BSL.ByteString) where
    fdInvoke func =
        fdInvoke iofunc
            where iofunc :: () -> IO BSL.ByteString
                  iofunc = return . func

instance ShellCommand (BS.ByteString -> BS.ByteString) where
    fdInvoke func =
        fdInvoke iofunc
            where iofunc :: BS.ByteString -> IO BS.ByteString
                  iofunc = return . func

instance ShellCommand (() -> BS.ByteString) where
    fdInvoke func =
        fdInvoke iofunc
            where iofunc :: () -> IO BS.ByteString
                  iofunc = return . func

instance ShellCommand (Handle -> Handle -> IO ()) where
    fdInvoke func fstdin fstdout childclosefds childfunc =
        do p <- try (forkProcess childstuff)
           pid <- case p of
                    Right x -> return x
                    Left x -> fail $ "Error in fork for func: " ++ show x
           return $ seq pid pid
           return [(show func,
                    getProcessStatus True False pid >>=
                                     (return . forceMaybe))]
        where childstuff = do closefds childclosefds [fstdin, fstdout]
                              hr <- fdToHandle fstdin
                              hw <- fdToHandle fstdout
                              childfunc
                              func hr hw
                              hClose hr
                              hClose hw

genericStringlikeIO :: (Show (a -> IO a)) => 
                       (Handle -> IO a) 
                    -> (Handle -> a -> IO ()) 
                    -> (a -> IO a) 
                    -> Fd
                    -> Fd 
                    -> [Fd] 
                    -> (IO ()) 
                    -> IO [InvokeResult]
genericStringlikeIO getcontentsfunc hputstrfunc func fstdin fstdout childclosefds childfunc =
    do r <- fdInvoke realfunc fstdin fstdout childclosefds childfunc
       return $ map (\(_, y) -> (show func, y)) r
    where realfunc :: Handle -> Handle -> IO ()
          realfunc hr hw = do hSetBuffering hw LineBuffering
                              d $ "SIOSFC Running func in child"
                              contents <- getcontentsfunc hr
                              d $ "SIOSFC Contents read"
                              result <- func contents
                              d $ "SIOSFC Func applied"
                              hputstrfunc hw result

genericStringlikeO :: (Show (() -> IO a)) =>
                      (Handle -> a -> IO ())
                   -> (() -> IO a)
                   -> Fd
                   -> Fd
                   -> [Fd]
                   -> (IO ())
                   -> IO [InvokeResult]
genericStringlikeO hputstrfunc func _ fstdout _ childfunc =
    do mv <- (newEmptyMVar::IO (MVar Bool))
       -- PipeCommand's implementation will close endpoints in the parent.
       -- We need to dup them so that they'll hang around.
       myfstdout <- dup fstdout
       i $ "\ngenericStringlikeO: fds " ++ show fstdout ++ " -> " ++ show myfstdout
       hw <- fdToHandle myfstdout
       forkIO (childthread mv hw)

       -- VERY IMPORTANT: if we don't yield here, then we may return and
       -- fds get closed before the child thread ever has a chance to run.
       -- Results in deadlock.
       -- The child will signal by loading up the MVar when we can proceed.
       yield
       takeMVar mv
       yield
       i $ "\ngenericSLO: after forkIO, before return"
       return [(show func,
                waitExit mv)]
    where childthread mv hw =
              do i $ "\ngenericStringlikeO thread start: " ++ show func
                 --hw <- fdToHandle myfstdout
                 hSetBuffering hw LineBuffering
                 i $ "genericStringlikeO signalling parent to proceed"
                 putMVar mv False
                 i $ "genericStringlikeO calling childfunc"
                 childfunc
                 i $ "genericStringlikeO calling func"
                 result <- func ()
                 i $ "genericStringlikeO calling hputstrfunc"
                 hputstrfunc hw result
                 i $ "genericStringlikeO calling hClose"
                 hClose hw
                 i $ "genericStringlikeO calling putMVar"
                 putMVar mv True
                 i $ "genericStringlikeO child thread exiting"
          i x = debugM "" x
          waitExit mv = do i $ "genericStringlikeO waitExit w"
                           takeMVar mv
                           i $ "genericStringlikeO waitExit returning"
                           return (Exited ExitSuccess)

instance Show ([String] -> [String]) where
    show _ = "([String] -> [String])"
instance Show (() -> [String]) where
    show _ = "(() -> [String])"
instance Show ([String] -> IO [String]) where
    show _ = "([String] -> IO [String])"
instance Show (() -> IO [String]) where
    show _ = "(() -> IO [String])"

{- | An instance of 'ShellCommand' for a pure Haskell function mapping
[String] to [String].

A [String] is generated from a Handle via the 'lines' function, and the
reverse occurs via 'unlines'.

So, this function is intended to operate upon lines of input and produce
lines of output. -}
instance ShellCommand ([String] -> [String]) where
    fdInvoke func = fdInvoke (unlines . func . lines)

instance ShellCommand (() -> [String]) where
    fdInvoke func = fdInvoke (unlines . func)

{- | The same for an IO function -}
instance ShellCommand ([String] -> IO [String]) where
    fdInvoke func = fdInvoke iofunc
        where iofunc input = do r <- func (lines input)
                                return (unlines r)

instance ShellCommand (() -> IO [String]) where
    fdInvoke func = fdInvoke iofunc
        where iofunc :: (() -> IO String)
              iofunc () = do r <- func ()
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

redir :: Fd -> Fd -> IO ()
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
    do d $ "closefds " ++ show uclosefds ++ " " ++ show noclosefds
       mapM_ closeit . filter (\x -> not (x `elem` noclosefds)) $ uclosefds
    where closeit fd = do d $ "Closing fd " ++ show fd
                          closeFd fd
          uclosefds = uniq inpclosefds

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

{- | Function to use when there is nothing for the child to do -}
nullChildFunc :: IO ()
nullChildFunc = return ()

{- | Different ways to get data from 'run'.

 * IO () runs, throws an exception on error, and sends stdout to stdout

 * IO String runs, throws an exception on error, reads stdout into
   a buffer, and returns it as a string.  Note: This output is not lazy.

 * IO [String] is same as IO String, but returns the results as lines.
   Note: this output is not lazy.

 * IO ProcessStatus runs and returns a ProcessStatus with the exit
   information.  stdout is sent to stdout.  Exceptions are not thrown.

 * IO (String, ProcessStatus) is like IO ProcessStatus, but also
   includes a description of the last command in the pipe to have
   an error (or the last command, if there was no error)

 * IO ByteString and are similar to their String counterparts.

 * IO (String, IO (String, ProcessStatus)) returns a String read lazily
   and an IO action that, when evaluated, finishes up the process and
   results in its exit status.

 * IO Int returns the exit code from a program directly.  If a signal
   caused the command to be reaped, returns 128 + SIGNUM.

 * IO Bool returns True if the program exited normally (exit code 0,
   not stopped by a signal) and False otherwise.

To address insufficient laziness, you can process anything that needs to be
processed lazily within the pipeline itself.
-}
class RunResult a where
    {- | Runs a command (or pipe of commands), with results presented
       in any number of different ways. -}
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
    run cmd = genericStringlikeResult hGetContents (\c -> evaluate (length c))
              cmd

instance RunResult (IO BSL.ByteString) where
    run cmd = genericStringlikeResult BSL.hGetContents 
              (\c -> evaluate (BSL.length c))
              cmd

instance RunResult (IO BS.ByteString) where
    run cmd = genericStringlikeResult BS.hGetContents
              (\c -> evaluate (BS.length c))
              cmd

instance RunResult (IO (String, IO (String, ProcessStatus))) where
    run cmd = intermediateStringlikeResult hGetContents cmd

instance RunResult (IO (BSL.ByteString, IO (String, ProcessStatus))) where
    run cmd = intermediateStringlikeResult BSL.hGetContents cmd

instance RunResult (IO (BS.ByteString, IO (String, ProcessStatus))) where
    run cmd = intermediateStringlikeResult BS.hGetContents cmd

intermediateStringlikeResult :: ShellCommand b =>
                                (Handle -> IO a)
                             -> b
                             -> IO (a, IO (String, ProcessStatus))
intermediateStringlikeResult hgetcontentsfunc cmd =
        do (pread, pwrite) <- createPipe
           -- d $ "runS: new pipe endpoints: " ++ show [pread, pwrite]
           -- d "runS 1"
           r <- fdInvoke cmd stdInput pwrite [pread, pwrite] nullChildFunc
           -- d $ "runS 2 closing " ++ show pwrite
           closeFd pwrite
           -- d "runS 3"
           hread <- fdToHandle pread
           hSetBuffering hread NoBuffering
           -- d "runS 4"
           c <- hgetcontentsfunc hread
           -- d "runS 5"
           return (c, processResults r)

genericStringlikeResult :: ShellCommand b => 
                           (Handle -> IO a)
                        -> (a -> IO c)
                        -> b 
                        -> IO a
genericStringlikeResult hgetcontentsfunc evalfunc cmd =
        do (c, r) <- intermediateStringlikeResult hgetcontentsfunc cmd
           evalfunc c
           --evaluate (length c)
           -- d "runS 6"
           -- d "runS 7"
           r >>= checkResults
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

{- | A convenience function.  Refers only to the version of 'run'
that returns @IO ()@.  This prevents you from having to cast to it
all the time when you do not care about the result of 'run'.

The implementation is simply:

>runIO :: (ShellCommand a) => a -> IO ()
>runIO = run
-}
runIO :: (ShellCommand a) => a -> IO ()
runIO = run

{- | Another convenience function.  This returns the first line of the output,
with any trailing newlines or whitespace stripped off.  No leading whitespace
is stripped.  This function will raise an exception if there is not at least
one line of output.  Mnemonic: runSL means \"run single line\".

This command exists separately from 'run' because there is already a
'run' instance that returns a String, though that instance returns the
entirety of the output in that String. -}
runSL :: (ShellCommand a) => a -> IO String
runSL cmd =
    do r <- run cmd
       when (r == []) $ fail $ "runSL: no output received from " ++ show cmd
       return (rstrip . head $ r)
