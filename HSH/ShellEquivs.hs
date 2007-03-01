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
-}

module HSH.ShellEquivs(
                       catFrom,
                       catTo,
                       grep,
                       grepV,
                       egrep,
                       egrepV,
                       tee,
                       wcL,
                      ) where

import HSH.Command
import Data.List
import Text.Regex
import Control.Monad
import Control.Exception(evaluate)

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

{- | Takes input, writes it to all the specified files, and passes it on.

This function buffers the input. 

See also 'catFrom'. -}
tee :: [FilePath] -> String -> IO String
tee [] inp = return inp
tee (x:xs) inp = do writeFile x inp
                    tee xs inp

{- | Search for the string in the lines.  Return those that match. -}
grep :: String -> [String] -> [String]
grep needle = filter (isInfixOf needle)

{- | Search for the string in the lines.  Return those that do NOT match. -}
grepV :: String -> [String] -> [String]
grepV needle = filter (not . isInfixOf needle)

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

{- | Count number of lines.  wc -l -}
wcL :: [String] -> [String]
wcL inp = [show $ genericLength inp]

