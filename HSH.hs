{- HSH -- The Haskell Shell
Copyright (C) 2004-2007 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

{- |
   Module     : HSH
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU LGPL, version 2.1 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Copyright (c) 2006 John Goerzen, jgoerzen\@complete.org

Welcome to HSH, the Haskell Shell infrastructure.

<http://software.complete.org/hsh>

HSH is designed to let you mix and match shell expressions with Haskell
programs.

Here are a few examples to get you started:

>run $ "echo /etc/pass*" :: IO String
> -> "/etc/passwd /etc/passwd-"
>
>runIO $ "ls -l" -|- "wc -l"
> -> 12
>
>runIO $ "ls -l" -|- wcL
> -> 12
>
>runIO $ ("ls", ["-l", "file with spaces.txt"])
>glob "~jgoerzen" >>= cd . head

wcL is a pure Haskell function defined in "HSH.ShellEquivs.wcL" as:

>wcL :: [String] -> [String]
>wcL inp = [show $ genericLength inp]

Here's another example:

> let countLines = (zipWith (\i line -> printf "%-5d %s" i line) 
>      [(1::Int)..])::([String] -> [String])
>
> runIO $ ("ls", ["-l"]) -|- countLines -|- filter (isSuffixOf "hs")
>   6     -rw-r--r-- 1 jgoerzen jgoerzen  1285 Jun  6 09:43 HSH.hs
>   11    -rw-r--r-- 1 jgoerzen jgoerzen   565 Jun  6 09:43 test.hs

To use HSH, you\'ll just want to import the HSH module.  To learn more,
please see the information in "HSH.Command" and "HSH.ShellEquivs".

You can run a command with HSH in several ways:

 * By using 'run' in a context that expects IO (), which will leave
   the final standard output going
   to the normal standard output of the program

 * By using 'run' in a context that expects a String, which will
   capture standard output into a buffer and present it as a String

 * Any of the numerous other methods documented in 'RunResult'.

 * The shortcut functions 'runIO' and 'runSL'.  'runIO' lets you run 
   a command and force the context IO (), which is a frequently-useful
   shortcut when you don't care about the result.  'runSL' grabs the
   first line of output in the result.

You can then specify a command, which could be a single command or a command
joined together with pipes.

There are many different items that make valid types; see the list of 
instances of 'ShellCommand' for a full list.  Here are a few:

 * A simple bare string is passed to the shell for execution.  The shell
   will then typically expand wildcards, parse parameters, etc.

 * A @(String, [String])@ tuple.  The first item in the tuple gives
   the name of a program to run, and the second gives its arguments.
   The shell is never involved.  This is ideal for passing filenames,
   since there is no security risk involving special shell characters.

 * A Haskell function.  This function will accept input representing
   its standard input and generate output to go to stdout.  Function
   types that are supported natively include @(String -> String)@,
   @(String -> IO String)@, plus many more involving ByteStrings and functions
   that take no input.  See 'HSH.Command.ShellCommand' for more.
@([String] -> [String])@, 
   @([String] -> IO [String])@.  Those that accept a @[String]@ type will
   have each string in the list representing a single line.

Pipes can be constructed by using the -|- operator, as illustrated above.
It is quite possible to pipe data between Haskell functions and
shell commands at will.

In addition, "HSH.ShellEquivs" provides a number of useful pure-Haskell
equivalents of regular shell commands.

For more information, please consult the other modules in HSH as well as
the HSH wiki at:

<http://software.complete.org/hsh>
-}

module HSH (
            module HSH.Command,
            module HSH.ShellEquivs
           )
where
import HSH.Command
import HSH.ShellEquivs