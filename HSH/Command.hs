{-# OPTIONS_GHC -XFlexibleInstances -XTypeSynonymInstances -XStandaloneDeriving #-}

{- Commands for HSH
Copyright (C) 2004-2008 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

{- |
   Module     : HSH.Command
   Copyright  : Copyright (C) 2006-2009 John Goerzen
   License    : GNU LGPL, version 2.1 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Copyright (c) 2006-2009 John Goerzen, jgoerzen\@complete.org
-}

module HSH.Command (Environment,
                    ShellCommand(..),
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
                    setenv,
                    unsetenv
                   ) where

-- import System.IO.HVIO
-- import System.IO.Utils
import Prelude hiding (catch)
import System.IO
import System.Exit
import System.Log.Logger
import System.IO.Error (isUserError, ioeGetErrorString)
import Data.Maybe.Utils
import Data.Maybe
import Data.List.Utils(uniq)
import Control.Exception(try, evaluate, SomeException, catch)
import Text.Regex.Posix
import Control.Monad(when)
import Data.String.Utils(rstrip)
import Control.Concurrent
import System.Process
import System.Environment(getEnvironment)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import HSH.Channel

d, dr :: String -> IO ()
d = debugM "HSH.Command"
dr = debugM "HSH.Command.Run"
em = errorM "HSH.Command"

{- | Result type for shell commands.  The String is the text description of
the command, not its output. -}
type InvokeResult = (String, IO ExitCode)

{- | Type for the environment. -}
type Environment = Maybe [(String, String)]

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
             -> Environment     -- ^ The environment
             -> Channel         -- ^ Where to read input from
             -> IO (Channel, [InvokeResult]) -- ^ Returns an action that, when evaluated, waits for the process to finish and returns an exit code.

instance Show (Handle -> Handle -> IO ()) where
    show _ = "(Handle -> Handle -> IO ())"
instance Show (Channel -> IO Channel) where
    show _ = "(Channel -> IO Channel)"
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
    fdInvoke = genericStringlikeIO chanAsString

{- | A user function that takes no input, and generates output.  We will deal
with it using hPutStr to send the output on. -}
instance ShellCommand (() -> IO String) where
    fdInvoke = genericStringlikeO

instance ShellCommand (BSL.ByteString -> IO BSL.ByteString) where
    fdInvoke = genericStringlikeIO chanAsBSL

instance ShellCommand (() -> IO BSL.ByteString) where
    fdInvoke = genericStringlikeO

instance ShellCommand (BS.ByteString -> IO BS.ByteString) where
    fdInvoke = genericStringlikeIO chanAsBS

instance ShellCommand (() -> IO BS.ByteString) where
    fdInvoke = genericStringlikeO

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

instance ShellCommand (Channel -> IO Channel) where
    fdInvoke func _ cstdin =
        runInHandler (show func) (func cstdin)

{-
instance ShellCommand (Handle -> Handle -> IO ()) where
    fdInvoke func cstdin cstdout =
        runInHandler (show func) (func hstdin hstdout)
-}

genericStringlikeIO :: (Show (a -> IO a), Channelizable a) =>
                       (Channel -> IO a)
                    -> (a -> IO a)
                    -> Environment
                    -> Channel
                    -> IO (Channel, [InvokeResult])
genericStringlikeIO dechanfunc userfunc _ cstdin =
    do contents <- dechanfunc cstdin
       runInHandler (show userfunc) (realfunc contents)
    where realfunc contents = do r <- userfunc contents
                                 return (toChannel r)

genericStringlikeO :: (Show (() -> IO a), Channelizable a) =>
                      (() -> IO a)
                   -> Environment
                   -> Channel
                   -> IO (Channel, [InvokeResult])
genericStringlikeO userfunc _ _ =
    runInHandler (show userfunc) realfunc
        where realfunc :: IO Channel
              realfunc = do r <- userfunc ()
                            return (toChannel r)

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
    fdInvoke (fp, args) = genericCommand (RawCommand fp args)

{- | An instance of 'ShellCommand' for an external command.  The
String is split using words to the command to run, and the arguments, if any. -}
instance ShellCommand String where
    fdInvoke cmd = genericCommand (ShellCommand cmd)

{- | How to we handle and external command. -}
genericCommand :: CmdSpec 
               -> Environment
               -> Channel
               -> IO (Channel, [InvokeResult])

-- Handling external command when stdin channel is a Handle
genericCommand c environ (ChanHandle ih) =
    let cp = CreateProcess {cmdspec = c,
                            cwd = Nothing,
                            env = environ,
                            std_in = UseHandle ih,
                            std_out = CreatePipe,
                            std_err = Inherit,
                            close_fds = True
#if MIN_VERSION_process(1,1,0)
-- Or use GHC version as a proxy:  __GLASGOW_HASKELL__ >= 720
			    -- Added field in process 1.1.0.0:
			    , create_group = False
#endif
#if MIN_VERSION_process(1,2,0)
			    , delegate_ctlc = False
#endif
#if MIN_VERSION_process(1,3,0)
          , detach_console = False
          , create_new_console = False
          , new_session = False
#endif
#if MIN_VERSION_process(1,4,0)
          , child_group = Nothing
          , child_user = Nothing
#endif
#if MIN_VERSION_process(1,5,0)
          , use_process_jobs = False
#endif
			   }
    in do (_, oh', _, ph) <- createProcess cp
          let oh = fromJust oh'
          return (ChanHandle oh, [(printCmdSpec c, waitForProcess ph)])
genericCommand cspec environ ichan = 
    let cp = CreateProcess {cmdspec = cspec,
                            cwd = Nothing,
                            env = environ,
                            std_in = CreatePipe,
                            std_out = CreatePipe,
                            std_err = Inherit,
                            close_fds = True
#if MIN_VERSION_process(1,1,0)
			    -- Added field in process 1.1.0.0:
			    , create_group = False
#endif
#if MIN_VERSION_process(1,2,0)
			    , delegate_ctlc = False
#endif
#if MIN_VERSION_process(1,3,0)
          , detach_console = False
          , create_new_console = False
          , new_session = False
#endif
#if MIN_VERSION_process(1,4,0)
          , child_group = Nothing
          , child_user = Nothing
#endif
#if MIN_VERSION_process(1,5,0)
          , use_process_jobs = False
#endif
			   }
    in do (ih', oh', _, ph) <- createProcess cp
          let ih = fromJust ih'
          let oh = fromJust oh'
          chanToHandle True ichan ih
          return (ChanHandle oh, [(printCmdSpec cspec, waitForProcess ph)])

printCmdSpec :: CmdSpec -> String
printCmdSpec (ShellCommand s) = s
printCmdSpec (RawCommand fp args) = show (fp, args)

------------------------------------------------------------
-- Pipes
------------------------------------------------------------

data PipeCommand a b = (ShellCommand a, ShellCommand b) => PipeCommand a b

deriving instance Show (PipeCommand a b)

{- | An instance of 'ShellCommand' represeting a pipeline. -}
instance (ShellCommand a, ShellCommand b) => ShellCommand (PipeCommand a b) where
    fdInvoke (PipeCommand cmd1 cmd2) env ichan =
        do (chan1, res1) <- fdInvoke cmd1 env ichan
           (chan2, res2) <- fdInvoke cmd2 env chan1
           return (chan2, res1 ++ res2)

{- | Pipe the output of the first command into the input of the second. -}
(-|-) :: (ShellCommand a, ShellCommand b) => a -> b -> PipeCommand a b
(-|-) = PipeCommand

{- | Different ways to get data from 'run'.

 * IO () runs, throws an exception on error, and sends stdout to stdout

 * IO String runs, throws an exception on error, reads stdout into
   a buffer, and returns it as a string.  Note: This output is not lazy.

 * IO [String] is same as IO String, but returns the results as lines.
   Note: this output is not lazy.

 * IO ExitCode runs and returns an ExitCode with the exit
   information.  stdout is sent to stdout.  Exceptions are not thrown.

 * IO (String, ExitCode) is like IO ExitCode, but also
   includes a description of the last command in the pipe to have
   an error (or the last command, if there was no error).

 * IO ByteString and are similar to their String counterparts.

 * IO (String, IO (String, ExitCode)) returns a String read lazily
   and an IO action that, when evaluated, finishes up the process and
   results in its exit status.  This command returns immediately.

 * IO (IO (String, ExitCode)) sends stdout to stdout but returns
   immediately.  It forks off the child but does not wait for it to finish.
   You can use 'checkResults' to wait for the finish.

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

instance RunResult (IO (String, ExitCode)) where
    run cmd =
        do (ochan, r) <- fdInvoke cmd Nothing (ChanHandle stdin)
           chanToHandle False ochan stdout
           processResults r

instance RunResult (IO ExitCode) where
    run cmd = ((run cmd)::IO (String, ExitCode)) >>= return . snd

instance RunResult (IO Int) where
    run cmd = do rc <- run cmd
                 case rc of
                   ExitSuccess -> return 0
                   ExitFailure x -> return x

instance RunResult (IO Bool) where
    run cmd = do rc <- run cmd
                 return ((rc::Int) == 0)

instance RunResult (IO [String]) where
    run cmd = do r <- run cmd
                 return (lines r)

instance RunResult (IO String) where
    run cmd = genericStringlikeResult chanAsString (\c -> evaluate (length c))
              cmd

instance RunResult (IO BSL.ByteString) where
    run cmd = genericStringlikeResult chanAsBSL
              (\c -> evaluate (BSL.length c))
              cmd

instance RunResult (IO BS.ByteString) where
    run cmd = genericStringlikeResult chanAsBS
              (\c -> evaluate (BS.length c))
              cmd

instance RunResult (IO (String, IO (String, ExitCode))) where
    run cmd = intermediateStringlikeResult chanAsString cmd

instance RunResult (IO (BSL.ByteString, IO (String, ExitCode))) where
    run cmd = intermediateStringlikeResult chanAsBSL cmd

instance RunResult (IO (BS.ByteString, IO (String, ExitCode))) where
    run cmd = intermediateStringlikeResult chanAsBS cmd

instance RunResult (IO (IO (String, ExitCode))) where
    run cmd = do (ochan, r) <- fdInvoke cmd Nothing (ChanHandle stdin)
                 chanToHandle False ochan stdout
                 return (processResults r)

intermediateStringlikeResult :: ShellCommand b =>
                                (Channel -> IO a)
                             -> b
                             -> IO (a, IO (String, ExitCode))
intermediateStringlikeResult chanfunc cmd =
        do (ochan, r) <- fdInvoke cmd Nothing (ChanHandle stdin)
           c <- chanfunc ochan
           return (c, processResults r)

genericStringlikeResult :: ShellCommand b => 
                           (Channel -> IO a)
                        -> (a -> IO c)
                        -> b 
                        -> IO a
genericStringlikeResult chanfunc evalfunc cmd =
        do (c, r) <- intermediateStringlikeResult chanfunc cmd
           evalfunc c
           --evaluate (length c)
           -- d "runS 6"
           -- d "runS 7"
           r >>= checkResults
           -- d "runS 8"
           return c

{- | Evaluates the result codes and returns an overall status -}
processResults :: [InvokeResult] -> IO (String, ExitCode)
processResults r =
    do rc <- mapM procresult r
       case catMaybes rc of
         [] -> return (fst (last r), ExitSuccess)
         x -> return (last x)
    where procresult :: InvokeResult -> IO (Maybe (String, ExitCode))
          procresult (cmd, action) =
              do rc <- action
                 return $ case rc of
                   ExitSuccess -> Nothing
                   x -> Just (cmd, x)

{- | Evaluates result codes and raises an error for any bad ones it finds. -}
checkResults :: (String, ExitCode) -> IO ()
checkResults (cmd, ps) =
       case ps of
         ExitSuccess -> return ()
         ExitFailure x ->
             fail $ cmd ++ ": exited with code " ++ show x
{- FIXME: generate these again 
         Terminated sig ->
             fail $ cmd ++ ": terminated by signal " ++ show sig
         Stopped sig ->
             fail $ cmd ++ ": stopped by signal " ++ show sig
-}

{- | Handle an exception derived from a program exiting abnormally -}
tryEC :: IO a -> IO (Either ExitCode a)
tryEC action =
    do r <-  Control.Exception.try action
       case r of
         Left ioe ->
          if isUserError ioe then
              case (ioeGetErrorString ioe =~~ pat) of
                Nothing -> ioError ioe -- not ours; re-raise it
                Just e -> return . Left . procit $ e
          else ioError ioe      -- not ours; re-raise it
         Right result -> return (Right result)
    where pat = ": exited with code [0-9]+$|: terminated by signal ([0-9]+)$|: stopped by signal [0-9]+"
          procit :: String -> ExitCode
          procit e
              | e =~ "^: exited" = ExitFailure (str2ec e)
--              | e =~ "^: terminated by signal" = Terminated (str2ec e)
--              | e =~ "^: stopped by signal" = Stopped (str2ec e)
              | otherwise = error "Internal error in tryEC"
          str2ec e =
              read (e =~ "[0-9]+$")

{- | Catch an exception derived from a program exiting abnormally -}
catchEC :: IO a -> (ExitCode -> IO a) -> IO a
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


{- | Convenience function to wrap a child thread.  Kicks off the thread, handles
running the code, traps execptions, the works.

Note that if func is lazy, such as a getContents sort of thing,
the exception may go uncaught here.

NOTE: expects func to be lazy!
 -}
runInHandler :: String           -- ^ Description of this function
            -> (IO Channel)     -- ^ The action to run in the thread
            -> IO (Channel, [InvokeResult])
runInHandler descrip func =
    catch (realfunc) (exchandler)
    where realfunc = do r <- func
                        return (r, [(descrip, return ExitSuccess)])
          exchandler :: SomeException -> IO (Channel, [InvokeResult])
          exchandler e = do em $ "runInHandler/" ++ descrip ++ ": " ++ show e
                            return (ChanString "", [(descrip, return (ExitFailure 1))])


------------------------------------------------------------
-- Environment
------------------------------------------------------------
{- | An environment variable filter function.

This is a low-level interface; see 'setenv' and 'unsetenv' for more convenient
interfaces. -}
type EnvironFilter = [(String, String)] -> [(String, String)]

instance Show EnvironFilter where
    show _ = "EnvironFilter"


{- | A command that carries environment variable information with it.

This is a low-level interface; see 'setenv' and 'unsetenv' for more
convenient interfaces. -}
data EnvironCommand a = (ShellCommand a) => EnvironCommand EnvironFilter a

deriving instance Show (EnvironCommand a)

instance (ShellCommand a) => ShellCommand (EnvironCommand a) where
    fdInvoke (EnvironCommand efilter cmd) Nothing ichan =
        do -- No incoming environment; initialize from system default.
           e <- getEnvironment
           fdInvoke cmd (Just (efilter e)) ichan
    fdInvoke (EnvironCommand efilter cmd) (Just ienv) ichan =
        fdInvoke cmd (Just (efilter ienv)) ichan

{- | Sets an environment variable, replacing an existing one if it exists.

Here's a sample ghci session to illustrate.  First, let's see the defaults for
some variables:

> Prelude HSH> runIO $ "echo $TERM, $LANG"
> xterm, en_US.UTF-8

Now, let's set one:

> Prelude HSH> runIO $ setenv [("TERM", "foo")] $ "echo $TERM, $LANG"
> foo, en_US.UTF-8

Or two:

> Prelude HSH> runIO $ setenv [("TERM", "foo")] $ setenv [("LANG", "de_DE.UTF-8")] $ "echo $TERM, $LANG"
> foo, de_DE.UTF-8

We could also do it easier, like this:

> Prelude HSH> runIO $ setenv [("TERM", "foo"), ("LANG", "de_DE.UTF-8")] $ "echo $TERM, $LANG"
> foo, de_DE.UTF-8

It can be combined with unsetenv:

> Prelude HSH> runIO $ setenv [("TERM", "foo")] $ unsetenv ["LANG"] $ "echo $TERM, $LANG"
> foo,

And used with pipes:

> Prelude HSH> runIO $ setenv [("TERM", "foo")] $ "echo $TERM, $LANG" -|- "tr a-z A-Z"
> FOO, EN_US.UTF-8

See also 'unsetenv'.
-}
setenv :: (ShellCommand cmd) => [(String, String)] -> cmd -> EnvironCommand cmd
setenv items cmd =
    EnvironCommand efilter cmd
    where efilter ienv = foldr efilter' ienv items
          efilter' (key, val) ienv = 
              (key, val) : (filter (\(k, _) -> k /= key) ienv)

{- | Removes an environment variable if it exists; does nothing otherwise.

See also 'setenv', which has a more extensive example.
-}
unsetenv :: (ShellCommand cmd) => [String] -> cmd -> EnvironCommand cmd
unsetenv keys cmd =
    EnvironCommand efilter cmd
    where efilter ienv = foldr efilter' ienv keys
          efilter' key = filter (\(k, _) -> k /= key)
