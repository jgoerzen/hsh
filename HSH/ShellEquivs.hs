{- Shell Equivalents
Copyright (C) 2004-2007 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

{- |
   Module     : HSH.ShellEquivs
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : GNU LGPL, version 2.1 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Copyright (c) 2006-2007 John Goerzen, jgoerzen\@complete.org

This module provides shell-like commands.  Most, but not all, are designed
to be used directly as part of a HSH pipeline.  All may be used outside
HSH entirely as well.

-}

module HSH.ShellEquivs(
                       abspath,
                       appendTo,
                       basename,
                       bracketCD,
                       catFrom,
                       catTo,
                       cd,
                       cut,
                       cutR,
                       dirname,
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
                       readlink,
                       readlinkabs,
                       rev,
                       rev_w,
                       space,
                       unspace,
                       tac,
                       tee,
                       tr,
                       trd,
                       wcW,
                       wcL,
                       uniq,
                      ) where

import Data.List
import Data.Char
import Text.Regex
import Text.Printf (printf)
import Control.Monad
import System.Directory hiding (createDirectory)
import System.Posix.Files
import System.Posix.User
import System.Posix.Directory
import System.Posix.Types
import System.IO.Error
import System.Path
import System.Exit
import qualified System.Path.Glob as Glob

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
basename = snd . splitpath

{- | The directory part of a path -}
dirname :: FilePath -> FilePath
dirname = fst . splitpath


{- | Changes the current working directory to the given path, executes
the given I\/O action, then changes back to the original directory,
even if the I\/O action raised an exception.

This is an alias for the MissingH function System.Path.bracketCWD.
-}
bracketCD :: FilePath -> IO a -> IO a
bracketCD = bracketCWD

{- | Load the specified files and display them, one at a time.

The special file @-@ means to display the input.

If it is not given, no input is read.

Unlike the shell cat, @-@ may be given twice.  However, if it is, you
will be forcing Haskell to buffer the input.

Note: buffering behavior here is untested.
-}
catFrom :: [FilePath] -> String -> IO String
catFrom fplist inp =
    do r <- foldM foldfunc "" fplist
       return r
    where foldfunc accum fp =
                  case fp of
                    "-" -> return (accum ++ inp)
                    fn -> do c <- readFile fn
                             return (accum ++ c)

{- | Takes input, writes it to the specified file, and does not pass it on.
     See also 'tee'. -}
catTo :: FilePath -> String -> IO String
catTo fp inp =
    do writeFile fp inp
       return ""

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

{- | Takes a string and sends it on as standard output.

The input to this function is never read. -}
echo :: String -> String
echo = id

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

Non-tilde expansion is done by the MissingH module System.Path.Glob.
-}
glob :: FilePath -> IO [FilePath]
glob inp@('~':remainder) =
    catch expanduser (\_ -> Glob.glob inp)
    where (username, rest) = span (/= '/') remainder
          expanduser =
              do lookupuser <-
                     if username /= ""
                        then return username
                        else getEffectiveUserName
                 ue <- getUserEntryForName lookupuser
                 Glob.glob (homeDirectory ue ++ rest)
glob x = Glob.glob x

{- | Search for the string in the lines.  Return those that match. -}
grep :: String -> [String] -> [String]
grep = filter . isInfixOf

{- | Search for the string in the lines.  Return those that do NOT match. -}
grepV :: String -> [String] -> [String]
grepV needle = filter (not . isInfixOf needle)


{- | Creates the given directory.  A value of 0o755 for mode would be typical.
An alias for System.Posix.Directory.createDirectory
-}
mkdir :: FilePath -> FileMode -> IO ()
mkdir = createDirectory

{- | An alias for System.Directory.getCurrentDirectory -}
pwd :: IO FilePath
pwd = getCurrentDirectory

{- | Return the destination that the given symlink points to.
An alias for System.Posix.Files.readSymbolicLink -}
readlink :: FilePath -> IO FilePath
readlink fp =
    do issym <- (getFileStatus fp >>= return . isSymbolicLink)
       if issym
           then readSymbolicLink fp
           else return fp

{- | As 'readlink', but turns the result into an absolute path. -}
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

splitpath :: String -> (String, String)
splitpath "" = (".", ".")
splitpath "/" = ("/", "/")
splitpath p
    | last p == '/' = splitpath (init p)
    | not ('/' `elem` p) = (".", p)
    | head p == '/' && length (filter (== '/') p) == 1 = ("/", tail p)
    | otherwise = (\(base, dir) -> (reverse (tail dir), reverse base))
        (break (== '/') (reverse p))

{- | Takes input, writes it to all the specified files, and passes it on.

This function buffers the input.

See also 'catFrom'. -}
tee :: [FilePath] -> String -> IO String
tee [] inp = return inp
tee (x:xs) inp = do writeFile x inp
                    tee xs inp

{- | Count number of lines.  wc -l -}
wcL, wcW :: [String] -> [String]
wcL inp = [show (genericLength inp :: Integer)]

-- | Count number of words in a file (like wc -w)
wcW inp = [show ((genericLength $ words $ unlines inp) :: Integer)]

{- | Split a list by a given character and select the nth list.
   > cut ' ' 2 "foo bar baz quux" -> "bar" -}
cut :: Integer -> Char -> String -> String
cut pos = flip cutR [pos]

{- | Split a list by a given character and select ranges of the resultant lists.
   > cutR ' ' [2..4] "foo bar baz quux foobar" -> "bar baz quux"
   Notes:
   * Needs to handle too large and too small indexes.
   * Might be better to use a Map, but that makes the function much longer for marginal
   performance benefit. -}
cutR :: Char -> [Integer] -> String -> String
cutR delim ns y = concat $ intersperse [delim] $ map (\z -> string `genericIndex` (z - 1)) ns
    where string = split delim y
          {- Utility function.
             > split ' ' "foo bar baz" -> ["foo","bar","baz"] -}
          split :: Char -> String -> [String]
          split c s = case rest of
	                   []     -> [chunk]
	                   _:rst -> chunk : split c rst
                    where (chunk, rest) = break (==c) s

-- | Join lines of a file
joinLines :: [String] -> [String]
joinLines = return . concat

-- | Number each line of a file
numberLines :: [String] -> [String]
numberLines = zipWith (printf "%3d %s") [(1::Int)..]


-- | Reverse characters on each line (rev)
rev, rev_w :: [String] -> [String]
rev = map reverse

-- | Reverse words on each line
rev_w = map (unwords . reverse . words)


-- | Double space a file
space, unspace :: [String] -> [String]
space = intersperse ""

-- | Inverse of double space; drop spaces
unspace = filter (not . null)


-- | Convert a string to all upper or lower case
lower, upper :: String -> String
lower = map toLower
upper = map toUpper


{- | Reverse lines in a String (like Unix tac).
See uniq. -}
tac :: String -> String
tac = unlines . reverse . lines

-- | Translate c character x to y, like tr 'e' 'f' (or y// in sed)
-- tr :: Char -> Char -> IO ()
tr :: Char -> Char -> String -> String
tr a b = map (\x -> if x == a then b else x)

-- | Delete specified character in a string.
trd :: Char -> String -> String
trd = filter . (/=)

{- | Remove duplicate lines from a file (like Unix uniq).
Takes a String representing a file or output and plugs it through lines and then nub to uniqify on a line basis. -}
uniq :: String -> String
uniq = unlines . nub . lines
