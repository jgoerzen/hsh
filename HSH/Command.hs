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

module HSH.Command ( ) where

import MissingH.Cmd
import MissingH.IO.HVIO
import MissingH.IO

{- | A shell command is something we can invoke, pipe to, pipe from,
or pipe in both directions.  All commands that can be run as shell
commands must define these methods. 

Any Handles passed in should be assumed to be closed by the functions in here.

Minimum implementation is 'pipeBoth'.

ALL THESE ARE GOING TO NEED TWO NEW ITEMS: one for parent and one for child...

but what about when we don't fork?

for the ones that don't fork, the items for 
 -}
class (Show a) => ShellCommand a where
    
    {- | Invoke a command, letting it receive input from standard in
       and send output to standard out, as usual. -}
    invoke :: a -> IO ()

    {- | Invoke a command, letting it receive input from standard in
       but directing its output to the designated handle. -}
    pipeFrom :: a -> Handle -> IO ()

    {- | Invoke a command, letting its output go to stdout,
       but receiving its input on the designated handle. -}
    pipeTo :: a -> Handle -> IO ()

    {- | Invoke a command, giving both a standard in and a standard
       out handle. -}
    pipeBoth :: a 
             -> Handle          -- ^ Input is provided to the command here
             -> Handle          -- ^ Output is received from the command here
             -> IO ()

    -- | Default invoke -- stdin and stdout
    invoke a = pipeBoth a stdin stdout

    pipeFrom a h = pipeBoth a stdin h
    pipeTo a h = pipeBoth a h stdout
    
{- | An instance of 'ShellCommand' for a pure Haskell function mapping
String to String. -}
instance ShellCommand (String -> String) where
    pipeBoth func inh outh = hInteract inh outh func

{- | An instance of 'ShellCommand' for a pure Haskell function mapping
[String] to [String].

A [String] is generated from a Handle via the 'lines' function, and the
reverse occurs via 'unlines'.

So, this function is intended to operate upon lines of input and produce
lines of output. -}
instance ShellCommand ([String] -> [String]) where
    pipeBoth func inh outh = hLineInteract inh outh func

{- | An instance of 'ShellCommand' for an external command.  The
first String is the command to run, and the list of Strings represents the
arguments to the program, if any. -}
instance ShellComand (String, [String]) where
    pipeBoth (cmd, args) inh outh =
        do fdi <- handleToFd inh
           fdo <- handleToFd outh
           pOpen3 (Just fdi) (Just fdo) Nothing cmd args (\_ -> return ())
                  (return ())

{- | Pipe the output of the first command into the input of the second. -}
(-|-) :: (ShellCommand a, ShellCommand b) => a -> b -> (a, b)

