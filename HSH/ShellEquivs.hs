{- Shell Equivalents
Copyright (C) 2004-2009 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

{- |
   Module     : HSH.ShellEquivs
   Copyright  : Copyright (C) 2009 John Goerzen
   License    : GNU LGPL, version 2.1 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Copyright (c) 2006-2009 John Goerzen, jgoerzen\@complete.org

This module provides shell-like commands.  Most, but not all, are designed
to be used directly as part of a HSH pipeline.  All may be used outside
HSH entirely as well.

-}

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#define __HSH_POSIX__
#else
#define __HSH_WINDOWS__
#endif

module HSH.ShellEquivs(
                       abspath,
                       appendTo,
                       basename,
                       bracketCD,
                       catFrom,
                       catBytes,
                       catBytesFrom,
                       catTo,
#ifdef __HSH_POSIX__
                       catToFIFO,
#endif
                       cd,
                       cut,
                       cutR,
                       dirname,
                       discard,
                       echo,
                       exit,
                       glob,
                       grep,
                       grepV,
                       egrep,
                       egrepV,
                       joinLines,
                       lower,
                       upper,
                       mkdir,
                       numberLines,
                       pwd,
#ifdef __HSH_POSIX__
                       readlink,
                       readlinkabs,
#endif
                       rev,
                       revW,
                       HSH.Command.setenv,
                       space,
                       unspace,
                       tac,
                       tee,
#ifdef __HSH_POSIX__
                       teeFIFO,
#endif
                       tr,
                       trd,
                       wcW,
                       wcL,
                       HSH.Command.unsetenv,
                       uniq,
                      ) where

import Data.List (genericLength, intersperse, isInfixOf, nub)
import Data.Char (toLower, toUpper)
import Text.Regex (matchRegex, mkRegex)
import Text.Printf (printf)
import Control.Monad (foldM)
import System.Directory hiding (createDirectory)
import Control.Exception(evaluate)
-- import System.FilePath (splitPath)

#ifdef __HSH_POSIX__
import System.Posix.Files (getFileStatus, isSymbolicLink, readSymbolicLink)
import System.Posix.User (getEffectiveUserName, getUserEntryForName, homeDirectory)
import System.Posix.Directory (createDirectory)
import System.Posix.Types (FileMode())
import System.Posix.IO
import System.Posix.Error
#endif

import System.Path (absNormPath, bracketCWD)
import System.Exit
import System.IO
import System.Process
import qualified System.Directory as SD
import qualified System.Path.Glob as Glob (glob)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import System.IO.Unsafe(unsafeInterleaveIO)
import HSH.Channel
import HSH.Command(setenv, unsetenv)

{- | Return the absolute path of the arg.  Raises an error if the
computation is impossible. -}
abspath :: FilePath -> IO FilePath
abspath inp =
    do p <- pwd
       case absNormPath p inp of
         Nothing -> fail $ "Cannot make " ++ show inp ++ " absolute within " ++
                    show p
         Just x -> return x

{- | The filename part of a path -}
basename :: FilePath -> FilePath
basename =  snd . splitpath

{- | The directory part of a path -}
dirname :: FilePath -> FilePath
dirname = fst . splitpath

{- | Changes the current working directory to the given path, executes
the given I\/O action, then changes back to the original directory,
even if the I\/O action raised an exception.

This is an alias for the MissingH function System.Path.bracketCWD. -}
bracketCD :: FilePath -> IO a -> IO a
bracketCD = bracketCWD

{- | Load the specified files and display them, one at a time.

The special file @-@ means to display the input.  If it is not given,
no input is processed at all.

@-@ may be given a maximum of one time.

See also 'catBytes' . -}
catFrom :: [FilePath] -> Channel -> IO Channel
catFrom fplist ichan =
    do r <- foldM foldfunc BSL.empty fplist
       return (toChannel r)
    where foldfunc accum fp =
                  case fp of
                    "-" -> do c <- chanAsBSL ichan
                              return (BSL.append accum c)
                    fn -> do c <- BSL.readFile fn
                             return (BSL.append accum c)

{- | Copy data from input to output, optionally with a fixed
maximum size, in bytes.  Processes data using ByteStrings internally,
so be aware of any possible UTF-8 conversions.

You may wish to use @hSetBuffering h (BlockBuffering Nothing)@ prior to calling
this function for optimal performance.

See also 'catFrom', 'catBytesFrom' -}
catBytes :: (Maybe Integer)    -- ^ Maximum amount of data to transfer
          -> Channel             -- ^ Handle for input
          -> IO Channel
catBytes count hr = catBytesFrom hr count hr

{- | Generic version of 'catBytes'; reads data from specified Channel, and
ignores stdin.
-}

catBytesFrom :: Channel          -- ^ Handle to read from
             -> (Maybe Integer)  -- ^ Maximum amount of data to transfer
             -> Channel          -- ^ Handle for input (ignored)
             -> IO Channel
catBytesFrom (ChanHandle hr) count cignore =
    case count of
         Nothing -> return (ChanHandle hr)
         Just m -> do c <- BSL.hGet hr (fromIntegral m)
                      return (ChanBSL c)
catBytesFrom cinput count cignore =
    case count of
      Nothing -> return cinput
      Just m -> do r <- chanAsBSL cinput
                   return (ChanBSL (BSL.take (fromIntegral m) r))

{- | Takes input, writes it to the specified file, and does not pass it on.
     The return value is the empty string.  See also 'catToBS', 
     'catToFIFO' -}
catTo :: FilePath -> Channel -> IO Channel
catTo fp ichan =
    do ofile <- openFile fp WriteMode
       chanToHandle ichan ofile
       hClose ofile
       return (ChanString "")

#ifdef __HSH_POSIX__

{- | Like 'catTo', but opens the destination in ReadWriteMode instead of
ReadOnlyMode.  Due to an oddity of the Haskell IO system, this is required
when writing to a named pipe (FIFO) even if you will never read from it.

This call will BLOCK all threads on open until a reader connects.

This is provided in addition to 'catTo' because you may want to cat to
something that you do not have permission to read from.

This function is only available on POSIX platforms.

See also 'catTo' -}
catToFIFO :: FilePath -> Channel -> IO Channel
catToFIFO fp ichan =
    do h <- fifoOpen fp
       chanToHandle ichan h
       hClose h
       return (ChanString "")

fifoOpen :: FilePath -> IO Handle
fifoOpen fp = 
    do fd <- throwErrnoPathIf (< 0) "HSH fifoOpen" fp $ 
             openFd fp WriteOnly Nothing defaultFileFlags
       fdToHandle fd

#endif

{- | Like 'catTo', but appends to the file. -}
appendTo :: FilePath -> String -> IO String
appendTo fp inp =
    do appendFile fp inp
       return ""

{- | An alias for System.Directory.setCurrentDirectory.

Want to change to a user\'s home directory?  Try this:

> glob "~jgoerzen" >>= cd . head

See also 'bracketCD'.
-}
cd :: FilePath -> IO ()
cd = setCurrentDirectory

{- | Split a list by a given character and select the nth list.

> cut ' ' 2 "foo bar baz quux" -> "bar"
-}
cut :: Integer -> Char -> String -> String
cut pos = cutR [pos]

{- | Read all input and produce no output.  Discards input completely. -}
discard :: Channel -> IO Channel
discard inh =
    do c <- chanAsBSL inh
       evaluate (BSL.length c)
       return (ChanString "")

{- | Split a list by a given character and select ranges of the resultant lists.

> cutR [2..4] ' ' "foo bar baz quux foobar" -> "baz quux foobar"
> cutR [1..1000] ' ' "foo bar baz quux foobar" -> "bar baz quux foobar"
> cutR [-1000..1000] ' ' "foo bar baz quux foobar" -> "foo bar baz quux foobar"

   Note that too large and too small indices are essentially ignored.
-}
cutR :: [Integer] -> Char -> String -> String
cutR nums delim z = drop 1 $ concat [delim:x | (x, y) <- zip string [0..], elem y nums]
     where string = split delim z

{- | Takes a string and sends it on as standard output.

The input to this function is never read.

You can pass this thing a String, a ByteString, or even a Handle.

See also 'echoBS'. -}
echo :: Channelizable a => a -> Channel -> IO Channel
echo inp _ = return . toChannel $ inp

{- | Search for the regexp in the lines.  Return those that match. -}
egrep :: String -> [String] -> [String]
egrep pat = filter (ismatch regex)
    where regex = mkRegex pat
          ismatch r inp = case matchRegex r inp of
                            Nothing -> False
                            Just _ -> True

{- | Search for the regexp in the lines.  Return those that do NOT match. -}
egrepV :: String -> [String] -> [String]
egrepV pat = filter (not . ismatch regex)
    where regex = mkRegex pat
          ismatch r inp = case matchRegex r inp of
                            Nothing -> False
                            Just _ -> True

{- | Exits with the specified error code. 0 indicates no error. -}
exit :: Int -> IO a
exit code
    | code == 0 = exitWith ExitSuccess
    | otherwise = exitWith (ExitFailure code)

{- | Takes a pattern.  Returns a list of names that match that pattern.
Handles:

>~username at beginning of file to expand to user's home dir
>? matches exactly one character
>* matches zero or more characters
>[list] matches any character in list
>[!list] matches any character not in list

The result of a tilde expansion on a nonexistant username is to do no
tilde expansion.

The tilde with no username equates to the current user.

Non-tilde expansion is done by the MissingH module System.Path.Glob. -}
glob :: FilePath -> IO [FilePath]
glob inp@('~':remainder) =
    catch expanduser (\_ -> Glob.glob rest)
    where (username, rest) = span (/= '/') remainder
#ifdef __HSH_POSIX__
          expanduser =
              do lookupuser <-
                     if username /= ""
                        then return username
                        else getEffectiveUserName
                 ue <- getUserEntryForName lookupuser
                 Glob.glob (homeDirectory ue ++ rest)
#else
          expanduser = fail "non-posix; will be caught above"
#endif
glob x = Glob.glob x

{- | Search for the string in the lines.  Return those that match.
Same as:

> grep needle = filter (isInfixOf needle)
-}
grep :: String -> [String] -> [String]
grep = filter . isInfixOf

{- | Search for the string in the lines.  Return those that do NOT match. -}
grepV :: String -> [String] -> [String]
grepV needle = filter (not . isInfixOf needle)

-- | Join lines of a file
joinLines :: [String] -> [String]
joinLines = return . concat

#ifdef __HSH_POSIX__
{- | Creates the given directory.  A value of 0o755 for mode would be typical.

An alias for System.Posix.Directory.createDirectory.

The second argument will be ignored on non-POSIX systems. -}
mkdir :: FilePath -> FileMode -> IO ()
mkdir = createDirectory
#else
mkdir :: FilePath -> a -> IO ()
mkdir fp _ = SD.createDirectory fp
#endif

{- | Number each line of a file -}
numberLines :: [String] -> [String]
numberLines = zipWith (printf "%3d %s") [(1::Int)..]

{- | An alias for System.Directory.getCurrentDirectory. -}
pwd :: IO FilePath
pwd = getCurrentDirectory

#ifdef __HSH_POSIX__
{- | Return the destination that the given symlink points to.

An alias for System.Posix.Files.readSymbolicLink

This function is only available on POSIX platforms. -}
readlink :: FilePath -> IO FilePath
readlink fp =
    do issym <- (getFileStatus fp >>= return . isSymbolicLink)
       if issym
           then readSymbolicLink fp
           else return fp

{- | As 'readlink', but turns the result into an absolute path.

This function is only available on POSIX platforms. -}
readlinkabs :: FilePath -> IO FilePath
readlinkabs inp =
       do issym <- (getFileStatus inp >>= return . isSymbolicLink)
          if issym
             then do rl <- readlink inp
                     case absNormPath (dirname inp) rl of
                       Nothing -> fail $ "Cannot make " ++ show rl ++ " absolute within " ++
                                  show (dirname inp)
                       Just x -> return x
             else abspath inp
#endif

{- | Reverse characters on each line (rev) -}
rev, revW :: [String] -> [String]
rev = map reverse

{- | Reverse words on each line -}
revW = map (unwords . reverse . words)

{- | Reverse lines in a String (like Unix tac).

Implemented as:

> tac = reverse

See 'uniq'. -}
tac :: [String] -> [String]
tac = reverse

{- | Takes input, writes it to all the specified files, and passes it on.
This function does /NOT/ buffer input.

See also 'catFrom'. -}
tee :: [FilePath] -> Channel -> IO Channel
tee fplist inp = teeBSGeneric (\fp -> openFile fp WriteMode) fplist inp

#ifdef __HSH_POSIX__
{- | FIFO-safe version of 'tee'.

This call will BLOCK all threads on open until a reader connects.

This function is only available on POSIX platforms. -}
teeFIFO :: [FilePath] -> Channel -> IO Channel
teeFIFO fplist inp = teeBSGeneric fifoOpen fplist inp
#endif

teeBSGeneric :: (FilePath -> IO Handle) 
             -> [FilePath] 
             -> Channel -> IO Channel
teeBSGeneric openfunc fplist ichan =
    do handles <- mapM openfunc fplist
       inp <- chanAsBSL ichan
       resultChunks <- hProcChunks handles (BSL.toChunks inp)
       return (ChanBSL $ BSL.fromChunks resultChunks)
    where hProcChunks :: [Handle] -> [BS.ByteString] -> IO [BS.ByteString]
          hProcChunks handles chunks = unsafeInterleaveIO $
              case chunks of
                [] -> do mapM_ hClose handles
                         return [BS.empty]
                (x:xs) -> do mapM_ (\h -> BS.hPutStr h x) handles
                             remainder <- hProcChunks handles xs
                             return (x : remainder)
    
{- | Translate a character x to y, like:

>tr 'e' 'f'

Or, in sed,

>y//
 -}
tr :: Char -> Char -> String -> String
tr a b = map (\x -> if x == a then b else x)

{- | Delete specified character in a string. -}
trd :: Char -> String -> String
trd = filter . (/=)

{- | Remove duplicate lines from a file (like Unix uniq).

Takes a String representing a file or output and plugs it through lines and then nub to uniqify on a line basis. -}
uniq :: String -> String
uniq = unlines . nub . lines

{- | Double space a file -}
space, unspace :: [String] -> [String]
space = intersperse ""

{- | Inverse of double space; drop empty lines -}
unspace = filter (not . null)

{- | Convert a string to all upper or lower case -}
lower, upper :: String -> String
lower = map toLower
upper = map toUpper

{- | Count number of lines.  wc -l -}
wcL, wcW :: [String] -> [String]
wcL inp = [show (genericLength inp :: Integer)]

{- | Count number of words in a file (like wc -w) -}
wcW inp = [show ((genericLength $ words $ unlines inp) :: Integer)]

{- Utility function.
> split ' ' "foo bar baz" -> ["foo","bar","baz"] -}
split :: Char -> String -> [String]
split c s = case rest of
              []     -> [chunk]
              _:rst -> chunk : split c rst
    where (chunk, rest) = break (==c) s

-- TODO: Perhaps simplify to make use of split
splitpath :: String -> (String, String)
splitpath "" = (".", ".")
splitpath "/" = ("/", "/")
splitpath p
    | last p == '/' = splitpath (init p)
    | not ('/' `elem` p) = (".", p)
    | head p == '/' && length (filter (== '/') p) == 1 = ("/", tail p)
    | otherwise = (\(base, dir) -> (reverse (tail dir), reverse base))
        (break (== '/') (reverse p))
