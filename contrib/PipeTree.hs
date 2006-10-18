module Main where 

-- TODO: 
--  document
--  unit tests
--  simplify checkStatus functions

import Control.Exception

import Data.IORef
import Data.List 
import Data.Typeable

import System.IO
import System.IO.Unsafe
import System.Posix.Directory
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types
import System.Exit
import System.Posix.Unistd

-- * Types

-- | A wrapper around a function that checks the return code of a
-- process
--
-- The function takes a boolean indicating if it should do a blocking
-- check or not
--
-- If the function is called in non-blocking mode and the process is
-- not done, the function will return a new function that should be used
-- to check the process status next time.
--
-- If the process is finished, the function will return a tuple of the
-- process description and the process status.
data CheckProcessStatus = CPS { unCPS :: Bool -> IO (Either CheckProcessStatus (String, ProcessStatus)) }

-- | See the function checkProcessStatus for more information
type CheckProcessStatusHandle = IORef CheckProcessStatus

-- | A wrapper around a process -- A process is not neccessarily a
-- single unix process.
--
-- The first argument to Process constructor is a user-friendly
-- description of the process
--
-- The second argument is a function that spawns the process. The
-- three arguments to the spawn function are file descriptors that the
-- spawned process should use for stdin, stdout, and stderr.
data Process = Process String (Fd -> Fd -> Fd -> IO CheckProcessStatus)

instance Show Process where
    show (Process desc _) = desc

-- * Type class for determining if a process exited successfully

-- in theory there could be processes whose return code is not of type
-- ProcessStatus... But, currently this is just overkill...

class Success r where
    success :: r -> IO Bool

instance (Success a) => Success (Maybe a) where
    success (Just s) = success s
    success Nothing = return False

instance Success ProcessStatus where
    success (Exited ExitSuccess) = return True
    success _ = return False

-- *  Type Class for turning things into 'Process'

class (Show a) => MkProcess a where
    mkProcess :: a -> Process

-- | A process is already a process, so we don't really doing anything
-- in that case
instance MkProcess Process where
    mkProcess p = p

-- * Pipes

-- | Pipelines (aka, 'ls /etc | cat -n') are built using the Pipe data
-- structure so that introspection is possible. This should make it
-- possible eventually do something like 'fmap log pipeline' to log
-- the output of each process in the pipeline...
data Pipe a b c = StdPipe a b
		| MergePipe a b
		| SplitPipe a (b, c)
		| RedirectStdin a FilePath
		| RedirectStdout a FilePath
		| AppendStdout a FilePath
		| RedirectStderr a FilePath
		| AppendStderr a FilePath
		| RedirectStdoutStderr a FilePath
		| AppendStdoutStderr a FilePath
		| Guardian a Guard

instance MkProcess () where
    mkProcess () =
	Process "()" $ \fdIn -> \fdOut -> \fdErr ->
			    do return (CPS $ \bool -> return $ Right $ ("()", Exited (ExitSuccess)))

data Guard = Guard String (CheckProcessStatus -> IO ())

instance (Show Guard) where
    show (Guard name _) = name

instance (Show a, Show b, Show c) => Show (Pipe a b c) where
    show (p1 `StdPipe` p2) = show p1 ++ " -|- " ++ show p2
    show (p1 `MergePipe` p2) = show p1 ++ " =|- " ++ show p2
    show (p1 `SplitPipe` (p2, p3)) = show p1 ++ " =|= ( " ++ show p2 ++", " ++ show p3 ++ " )"
    show (RedirectStdin p filepath) = show p ++ " <| " ++ filepath
    show (RedirectStdout p filepath) = show p ++ " |> " ++ filepath
    show (AppendStdout p filepath) = show p ++ " |>> " ++ filepath
    show (RedirectStderr p filepath) = show p ++ " @> " ++ filepath
    show (AppendStderr p filepath) = show p ++ " @>> " ++ filepath
    show (RedirectStdoutStderr p filepath) = show p ++ " |@> " ++ filepath
    show (AppendStdoutStderr p filepath) = show p ++ " |@>> " ++ filepath
    show (Guardian p g) = show p ++ " `guardian` " ++ show g

-- | This seems very complex, there is probably some way
-- to reduce the complexity and still get the same
-- answer
checkStatus3 :: String -> CheckProcessStatus -> CheckProcessStatus -> CheckProcessStatus -> CheckProcessStatus
checkStatus3 pipeName  p1@(CPS s1) p2@(CPS s2) p3@(CPS s3) =
    CPS $ \block ->
	do s1' <- s1 block
	   case s1' of
		Left cps1' ->
		    do s2' <- s2 block
		       case s2' of
			    Left cps2' ->
				do s3' <- s3 block
				   case s3' of
					Left cps3' -> return $ Left $ checkStatus3 pipeName cps1' cps2' cps3'
					Right (desc,status) -> checkRetCode (desc,status)
			    Right (desc, status) ->
				do success' <- success status
				   if success'
				      then do s3' <- s3 block
					      case s3' of
						   Left cps3' -> return $ Left $ checkStatus2 pipeName cps1' cps3'
						   Right (desc, status) -> checkRetCode (desc,status)
				      else return $ Right (desc, status)
		Right (desc, status) ->
		    do success' <- success status
		       if success'
			  then do s2' <- s2 block
				  case s2' of
				       Left cps2' ->
					   do s3' <- s3 block
					      case s3' of
						   Left cps3' -> return $ Left $ checkStatus2 pipeName cps2' cps3'
						   Right (desc,status) -> checkRetCode (desc,status)
				       Right (desc, status) ->
					   do success' <- success status
					      if success'
						 then do s3' <- s3 block
							 case s3' of
							      Left cps3' -> return $ Left $ checkStatus1 pipeName cps3'
							      Right (desc, status) -> checkRetCode (desc,status)
						 else return $ Right (desc,status)
			  else return $ Right (desc, status)
    where checkRetCode (desc,status) =
	      do success' <- success status
		 if success'
		    then return $ Right $ (pipeName, status)
		    else return $ Right $ (desc, status)

checkStatus2 :: String -> CheckProcessStatus -> CheckProcessStatus -> CheckProcessStatus
checkStatus2 pipeName p1@(CPS s1) p2@(CPS s2) =
		  CPS $ \block ->
		      do s1' <- s1 block
			 case s1' of
			      Left p1' -> return $ Left $ checkStatus2 pipeName p1' p2
			      Right (desc, status) ->
				  do status' <- success status 
				     if (not status')
					then return $ Right (desc, status)
					else do s2' <- s2 block
						case s2' of
						     Left s2'' -> return $ Left $ checkStatus1 pipeName s2''
						     Right (desc, status) ->
							 do status' <- success status
							    if (not status')
							       then return $ Right (desc, status)
							       else return $ Right (pipeName, status)

checkStatus1 :: String -> CheckProcessStatus -> CheckProcessStatus
checkStatus1 pipeName p@(CPS s) =
		  CPS $ \block ->
		      do res <- s block
			 case res of
			      Left s' -> return $ Left $ checkStatus1 pipeName s'
			      Right (desc, status) ->
				  do status' <- success status
				     if (not status')
					then return $ Right (desc, status)
					else return $ Right (pipeName, status)

check2 :: String -> CheckProcessStatus -> CheckProcessStatus -> CheckProcessStatus
check2 pipeName p1@(CPS cps1) p2@(CPS cps2) =
    CPS$ \block ->
	do status1 <- cps1 block
	   case status1 of
		Left cps1' ->
		    do status2 <- cps2 block
		       case status2 of
			    Left cps2' -> return $ Left $ check2 pipeName cps1' cps2'
			    Right (desc,status) ->
				do success' <- success status
				   if success'
				      then return $ Right $ (pipeName, status)
				      else return $ Right $ (desc, status)
		Right (desc,status) ->
		    do success' <- success status
		       if success'
			  then do status2 <- cps2 block
				  case status2 of
				       Left cps2' -> return $ Left $ checkStatus1 pipeName cps2'
				       Right (desc, status) ->
					   do success' <- success status
					      if success'
						 then return $ Right (pipeName, status)
						 else return $ Right (desc, status)
			  else return $ Right $ (desc,status)
		

apProc (Process _ p) = p

instance (MkProcess a, MkProcess b, MkProcess c) => MkProcess (Pipe a b c) where
    mkProcess p@(p1 `StdPipe` p2) = 
	Process (show p) $ \fdIn -> \fdOut -> \fdErr ->
				    do (pout, pin) <- createPipe
				       r1 <- apProc (mkProcess p1) fdIn pin stdError
				       closeFd pin
				       r2 <- apProc (mkProcess p2) pout fdOut fdErr
				       return $ check2 (show p) r1 r2

    mkProcess p@(p1 `MergePipe` p2) = 
	Process (show p) $ \fdIn -> \fdOut -> \fdErr ->
				    do (pout, pin) <- createPipe
				       r1 <- apProc (mkProcess p1) fdIn pin pin
				       closeFd pin
				       r2 <- apProc (mkProcess p2) pout fdOut fdErr
				       return $ check2 (show p) r1 r2

    mkProcess p@(p1 `SplitPipe` (p2, p3)) = 
	Process (show p) $ \fdIn -> \fdOut -> \fdErr ->
				    do (pout_stdout, pin_stdout) <- createPipe
				       (pout_stderr, pin_stderr) <- createPipe
				       r1 <- apProc (mkProcess p1) fdIn (pin_stdout) (pin_stderr)
				       closeFd pin_stdout
				       closeFd pin_stderr
				       r2 <- apProc (mkProcess p2) pout_stdout fdOut fdErr
				       r3 <- apProc (mkProcess p3) pout_stderr fdOut fdErr
				       return $ check2 (show p) r1 (check2 (show p) r2 r3)

    -- JAS - do something smarter if filepath does not exist
    mkProcess p@(RedirectStdin p1 filepath) =
	Process (show p) $ \fdIn -> \fdOut -> \fdErr ->
				    do fd <- openFd filepath ReadOnly Nothing defaultFileFlags
				       apProc (mkProcess p1) fd fdOut fdErr

    mkProcess p@(RedirectStdout p1 filepath) =
	Process (show p) $ \fdIn -> \fdOut -> \fdErr ->
				    do fd <- openFd filepath WriteOnly (Just 0o666) defaultFileFlags{ trunc=True }
				       apProc (mkProcess p1) fdIn fd fdErr

    mkProcess p@(AppendStdout p1 filepath) =
	Process (show p) $ \fdIn -> \fdOut -> \fdErr ->
				    do fd <- openFd filepath WriteOnly (Just 0o666) defaultFileFlags{ trunc=False, append=True }
				       apProc (mkProcess p1) fdIn fd fdErr

    mkProcess p@(RedirectStderr p1 filepath) =
	Process (show p) $ \fdIn -> \fdOut -> \fdErr ->
				    do fd <- openFd filepath WriteOnly (Just 0o666) defaultFileFlags{ trunc=True }
				       apProc (mkProcess p1) fdIn fdOut fd

    mkProcess p@(AppendStderr p1 filepath) =
	Process (show p) $ \fdIn -> \fdOut -> \fdErr ->
				    do fd <- openFd filepath WriteOnly (Just 0o666) defaultFileFlags{ trunc=False, append=True }
				       apProc (mkProcess p1) fdIn fdOut fd

    mkProcess p@(RedirectStdoutStderr p1 filepath) =
	Process (show p) $ \fdIn -> \fdOut -> \fdErr ->
				    do fd <- openFd filepath WriteOnly (Just 0o666) defaultFileFlags{ trunc=True }
				       apProc (mkProcess p1) fdIn fd fd

    mkProcess p@(AppendStdoutStderr p1 filepath) =
	Process (show p) $ \fdIn -> \fdOut -> \fdErr ->
				    do fd <- openFd filepath WriteOnly (Just 0o666) defaultFileFlags{ trunc=False, append=True }
				       apProc (mkProcess p1) fdIn fd fd

    mkProcess g@(Guardian p (Guard gname guard)) =
	let (Process pname process) = mkProcess p in
	    ioProcess (show g) $ do cps <- process stdInput stdOutput stdError
				    guard cps


-- * Pipe construction helper functions

-- | these functions build things of type Pipe using bash-like
-- operators for example:
--
--  bash    -> ls /etc | cat -n > /tmp/etc.log , could
--  haskell -> ls ["/etc"] -|- cat ["-n"] |> "/tmp/etc.log"

infixr 1 -|-
infixr 1 =|-
infixr 1 =|=
infixr 1 <|
infixr 1 |>
infixr 1 |>>
infixr 1 @>
infixr 1 @>>
infixr 1 |@>
infixr 1 |@>>

(-|-) :: (MkProcess a, MkProcess b) => a -> b -> Pipe a b ()
p1 -|- p2 = p1 `StdPipe` p2

(=|-) :: (MkProcess a, MkProcess b) => a -> b -> Pipe a b ()
p1 =|- p2 = p1 `MergePipe` p2

(=|=) :: (MkProcess a, MkProcess b, MkProcess c) => a -> (b, c) -> Pipe a b c
p1 =|= (p2, p3) = p1 `SplitPipe` (p2, p3)

-- hrm, is this type signature correct ? Without it we get:
--     Ambiguous type variable `b' in the constraint:
--      `MkProcess b' arising from use of `test' at <interactive>:1:1-4
--    Probable fix: add a type signature that fixes these type variable(s)
-- when we try to use the operator
(<|) :: (MkProcess a) => a -> FilePath -> Pipe a () ()
p <| filepath = RedirectStdin p filepath

(|>) :: (MkProcess a) => a -> FilePath -> Pipe a () ()
p |> filepath = RedirectStdout p filepath

(|>>) :: (MkProcess a) => a -> FilePath -> Pipe a () ()
p |>> filepath = AppendStdout p filepath

(@>) :: (MkProcess a) => a -> FilePath -> Pipe a () ()
p @> filepath = RedirectStderr p filepath

(@>>) :: (MkProcess a) => a -> FilePath -> Pipe a () ()
p @>> filepath = AppendStderr p filepath

(|@>) :: (MkProcess a) => a -> FilePath -> Pipe a () ()
p |@> filepath = RedirectStdoutStderr p filepath

(|@>>) :: (MkProcess a) => a -> FilePath -> Pipe a () ()
p |@>> filepath = AppendStdoutStderr p filepath

-- * Useful functions turning things into a 'Process'

-- | Turn an IO () into a Process
-- The IO () will be run in a forked process
-- JAS
-- is there some problem with pipes being open places they should not
-- be? I think because we fork the process, each process needs to
-- close the side of the pipe it does not use. Maybe there is a file
-- descriptor leak here.
ioProcess :: String -> IO () -> Process
ioProcess description io  =
    Process description $ \fdIn -> \fdOut -> \fdErr ->
				   do childPid <- forkProcess $ do dupTo fdIn stdInput
								   hSetBuffering stdin LineBuffering
								   dupTo fdOut stdOutput
								   hSetBuffering stdout LineBuffering
								   dupTo fdErr stdError
								   hSetBuffering stderr NoBuffering
								   -- in theory we should close fdIn, fdOut, and fdErr since they
								   -- are not used after the dupTo
								   -- but there are some possible caveats
								   -- http://www.unixwiz.net/techtips/remap-pipe-fds.html
								   closeFd fdIn
								   closeFd fdOut
								   closeFd fdErr
								   io
				      return $ checkStatus description childPid
    where checkStatus :: String -> ProcessID -> CheckProcessStatus
	  checkStatus description childPid =
	      CPS $ \block ->
		  do status <- getProcessStatus block False childPid
		     case status of
			  Nothing -> return $ Left $ checkStatus description childPid
			  Just status' -> return $ Right (description, status')

-- | Make a Process that executes a program from the disk
-- | see executeFile for more info on the arguments
fileProcess :: FilePath -- ^ program to execute
      -> Bool -- ^ search PATH?
      -> [String] -- ^ arguments
      -> Maybe [(String,String)] -- ^ environment
      -> Process
fileProcess prog search args menv = 
    ioProcess (concat (intersperse " " (prog:args))) $ 
	      do executeFile prog search args menv
		 error ("executeFile failed: " ++ prog)


-- | This is a varition of fileProcess that sets search path? to True,
-- and the enviroment to Nothing. It is useful to keep things readable
-- without having to define helper functions.
-- usage:  "cat" <<- ["/etc/motd"]
infix 2 <<-

(<<-) :: String -> [String] -> Process
pName <<- args = fileProcess pName True args Nothing


-- | filterProcess takes a function of type (String -> String) and
-- turns it into a Process, ex: filterProcess "reverse" reverse
filterProcess :: String  -- ^ Filter description
	      -> (String -> String) -- ^ Filter function
	      -> Process
filterProcess desc f = ioProcess desc $ interact f


-- * Executing a Process

-- | Run a 'Process' in the IO Monad
-- returns the a 3-tuple of (CheckProcessStatus, stdout, stderr)

-- WARNING: If you do a blocking check of the process status before
-- all of stdout and stderr have been consumed, you will likely end up
-- in a dead-lock.
runProcess :: MkProcess a => a -> IO (CheckProcessStatus, String, String)
runProcess p = 
    do let (Process desc p1) = mkProcess p
       (stdout_pout,stdout_pin) <- createPipe
       (stderr_pout,stderr_pin) <- createPipe
       r1 <- p1 (stdInput) (stdout_pin) (stderr_pin)
       stdout <- fdToHandle stdout_pout >>= \h -> hGetContents h
       stderr <- fdToHandle stderr_pout >>= \h -> hGetContents h
       closeFd stdout_pin
       closeFd stderr_pin
       return (r1, stdout, stderr)

-- | Run a 'Process' in the IO Monad
-- the process will use the same stdInput, stdOutput, and stdError as
-- the caller
-- returns CheckProcessStatus
startProcess :: MkProcess a => a -> IO CheckProcessStatus
startProcess p = 
    do let (Process desc p1) = mkProcess p
       r1 <- p1 stdInput stdOutput stdError
       return r1


-- * Higher Level User-friendly Functions

-- | The process wrapped inside of a CheckProcessStatus can only be
-- used to check the status once. If the process is not finished,
-- then a new function is returned that should be used to check the
-- status again.
--
-- There is nothing to prevent the caller from accidently calling the
-- original function twice. To avoid accidently making mistakes, we
-- canwrap the CheckProcessStatus in an IORef, so that it 'feels'
-- somewhat like a file handle. 
--
-- We can then use checkProcessStatus to safely check the status.
checkProcessStatus :: IORef (CheckProcessStatus) -> Bool -> IO (Maybe (String, ProcessStatus))
checkProcessStatus cpsR block =
    do cps <- readIORef cpsR
       res <- (unCPS cps) block
       case res of
	    Left cps' ->
		do writeIORef cpsR cps'
		   return Nothing
	    Right status ->
		do writeIORef cpsR (CPS $ \block -> return $ Right status)
		   return (Just status)

-- | similar to runProcess, except it returns a CheckProcessStatusHandle
runProcessH :: MkProcess a => a -> IO (CheckProcessStatusHandle, String, String)
runProcessH p =
    do (cps, stdout, stderr) <- runProcess p
       cpsH <- newIORef cps
       return $ (cpsH, stdout, stderr)

-- | Run 'p'. If it exits successfully, then call t, else call e.
ifP :: MkProcess a => a -> IO b -> IO b -> IO b
ifP p t e =
    do cps <- startProcess p
       res <- (unCPS cps) True
       case res of
	    Left _ -> error "A blocking call to a CPS returned a Left CPS :("
	    Right (name,status) ->
		do s <- success status
		   if s 
		      then t
		      else e

-- | You do not have to use thenP and elseP with ifP, but using them
-- | can help avoid parenthesis

infixl 1 `thenP`
infixl 1 `elseP`

f `thenP` x = f x
f `elseP` x = f x


-- | call startProcess on 'p'. 
-- If 'p' exits successfully, do nothing.
-- If 'p' fails, abort
-- NOTE: rename this function, as aborting does not sound very safe to me :)
safeStartProcess :: MkProcess a => a -> IO ()
safeStartProcess p =
    do let (Process desc _) = mkProcess p
       cps <- startProcess p
       res <- (unCPS cps) True
       case res of
	    Left _ -> error "A blocking call to a CPS returned a Left CPS :("
	    Right (name,status) ->
		do s <- success status
		   if s
		      then return ()
		      else error $ "`" ++ desc ++ "' failed with exit status `" ++ show status ++"'"

-- * Error handling

-- | guardian is used to wrap a guardian process around a process.

-- In short: the guardian has the same stdInput, stdOutput, and
-- stdError as the guarded process. If the guarded process dies, the
-- guardian can do things like restart the child, mangle the return
-- code, or simple dump extra debug information to stderr and then
-- propogate the error code.
--
-- The key is, the guardian process does this while keeping the
-- pipeline intact -- the other processes in the pipeline have no idea
-- the guarded process has died.
--
-- Note that if the guarded process read, but did not fully process
-- some data from stdin before dying, that information will be lost.
--
-- Developing/documentation is still being done on this function/usage.
guardian :: (MkProcess p) => p -> Guard -> Pipe p () ()
guardian = Guardian

-- * Misc (stuff that probably belongs elsewhere *)

-- | Change the working directory the IO () action is performed in
withWorkingDirectory :: IO () -- ^ action
		     -> FilePath -- ^ new working directory
		     -> IO ()
withWorkingDirectory action d = 
    do cwd <- getWorkingDirectory
       changeWorkingDirectory d
       action
       changeWorkingDirectory cwd

-- * Test Code

-- | define functions for calling some common utilities

ls args  = fileProcess "ls"  True args Nothing
cat args = fileProcess "cat" True args Nothing
find args = fileProcess "find" True args Nothing
stat args = fileProcess "stat" True args Nothing
test args = fileProcess "test" True args Nothing

-- | alias putStrLn to echo for a more bash-like experience
echo str = putStrLn str

-- | a simple example of constructing a pipeline
pipeline = ls ["/etc"] -|- cat ["-n"] -|- cat ["-E"]

-- | An example guard function for use with 'guardian'. 
-- This guard just prints the exit status of the guarded process to
-- stdout
simpleGuard :: Guard
simpleGuard =
    Guard "simpleGuard" $ \ (CPS cps) ->
	  do status <- cps True
	     case status of
		  Left e -> error "A blocking call to a CPS returned a Left CPS :("
		  Right (name,status) ->
		      do s <- success status
			 if s
			    then putStrLn $ name ++ " exited successfully."
			    else do putStrLn $ name ++ " made a boo-boo: " ++ show status
				    exitWith $ (\ (Exited e) -> e) status


-- | A simple 'IO ()' which can be used with ioProcess
hello :: IO ()
hello =
    do putStrLn "Hello, World!"
       putStrLn "Goodbye, World!"

-- | A process that always exits with failure
failProcess :: Process
failProcess = ioProcess "failProcess" $ exitWith (ExitFailure 2)

-- | Adds a prefix to the beginning of every line
tag :: String -> Process
tag str = filterProcess ("tag " ++ str) $ unlines . (map ((++) (str ++ " "))) . lines

-- | demo/test function
-- run a process and print stdout and stderr and the return code
runAndShow:: MkProcess p => p -> IO ()
runAndShow p = 
    do (res,out,err) <- runProcessH p
       checkCPS res False
       putStrLn out
       putStrLn err
       checkCPS res True

-- | demo/test function
-- check the process status and print the current status
checkCPS :: CheckProcessStatusHandle -> Bool -> IO ()
checkCPS res block =
    do status <- checkProcessStatus res block
       case status of
	    Nothing ->  
		do putStrLn "Process not finished yet"
	    Just (desc, status) ->
		do putStrLn (desc ++ " : " ++ show status)

simpleTest = 
    runAndShow $ (cat ["/etc/motd"])

-- | NOTE: there are some intentional failures in this example
complexTest =
    do ((safeRunProcess $  "ls" <<- ["/etc"] -|- cat ["-n2"]) >>= (\ (o,e) -> putStrLn (o ++ "\n" ++ e))) `catchDyn` showPipeExn
       -- demo of a guardian, note that the output the simpleGuard gets run through, cat ["-n"]
       runAndShow $ (ls ["/etc2"] `guardian` simpleGuard) -|- cat ["-n"]
       -- demo of ifP, change /etc/motd to a non-existant file if you want...
       ifP ("cat" <<- ["-n","/etc/motd"] -|- cat ["-E"] |> "/tmp/redirecttest")
	      `thenP` do echo "Found motd"
                         -- change "/etc" to a non-existant path to see safeStartProcess abort
			 safeStartProcess ("ls" <<- ["/etc"])
			 return ()
	      `elseP` echo "did not find motd"

main = 
    simpleTest

-- * Under development (stuff that does not work right yet)

-- | Run a 'Process' in the IO Monad
-- returns the a 3-tuple of (stdout, stderr)
-- If the process exits non-zero, then an exception is raised
safeRunProcess :: MkProcess a => a -> IO (String, String)
safeRunProcess p =
    do let (Process desc p1) = mkProcess p
       (stdout_pout,stdout_pin) <- createPipe
       (stderr_pout,stderr_pin) <- createPipe
       r1 <- p1 (stdInput) (stdout_pin) (stderr_pin)
       count  <- newIORef 2
       stdout <- fdToHandle stdout_pout >>= \h -> hGetContentsThen h (exceptionOnNonZero count r1)
       stderr <- fdToHandle stderr_pout >>= \h -> hGetContentsThen h (exceptionOnNonZero count r1)
       closeFd stdout_pin
       closeFd stderr_pin
       return (stdout, stderr)

data PipeExn = NonZeroExit (String, ProcessStatus)
	     deriving (Typeable, Show)

showPipeExn :: PipeExn -> IO ()
showPipeExn e@(NonZeroExit _) = putStrLn $ "Process Threw Exception:\n" ++ show e

exceptionOnNonZero :: IORef Int -> CheckProcessStatus -> IO ()
exceptionOnNonZero countRef (CPS cps) =
    do count <- readIORef countRef
       writeIORef countRef (count - 1)
       if (count /= 1)
	  then return ()
	  else do res <- cps True
		  case res of
		       Left e -> error "A blocking call to a CPS returned a Left CPS :("
		       Right res'@(_,status) ->
			   do success' <- success status
			      if success'
				 then return ()
				 else throwDyn (NonZeroExit res')

{-
exceptionOnNonZero :: CheckProcessStatus -> IO ()
exceptionOnNonZero (CPS cps) =
    do res <- cps True
       case res of
	    Left e -> error "A blocking call to a CPS returned a Left CPS :("
	    Right res'@(_,status) ->
		do success' <- success status
		   if success'
		      then return ()
		      else throwDyn (NonZeroExit res')
-}


-- | hGetContentsThen
-- similar to hGetContents, except it executes an action after 
-- the last character has been consumed.
--
--
-- It is not clear that this is actually safe, but time will tell. If
-- this does not work out, then we have to fiddle around under the
-- hood of hGetContents instead.
--
-- written by: Bastiaan Zapf, aka basti_
hGetContentsThen :: Handle -> IO a -> IO String
hGetContentsThen h after = 
    do x<-hGetContents h
       return (x++(unsafePerformIO $ do after
                                        return []))
