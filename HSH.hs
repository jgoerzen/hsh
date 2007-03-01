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

This module provides the basics that you will need for writing programs that
use HSH.
-}

module HSH (ShellCommand(..),
            run,
            runS,
            (-|-),
            InvokeResult,
            PipeCommand(..))
where
import HSH.Command