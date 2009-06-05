{-# OPTIONS_GHC -XFlexibleInstances -XTypeSynonymInstances #-}

{- Channel basics for HSH
Copyright (C) 2004-2008 John Goerzen <jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

{- |
   Module     : HSH.Channel
   Copyright  : Copyright (C) 2006-2009 John Goerzen
   License    : GNU LGPL, version 2.1 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Copyright (c) 2006-2009 John Goerzen, jgoerzen\@complete.org
-}

module HSH.Channel (Channel(..),
                    chanAsString,
                    chanAsBSL,
                    chanAsBS,
                    chanToHandle,
                    Channelizable(..)
                   ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSLC
import System.IO
import Control.Concurrent

{- | The main type for communicating between commands.  All are expected to
be lazy. -}
data Channel = ChanString String
             | ChanBSL BSL.ByteString
             | ChanHandle Handle

chanAsString :: Channel -> IO String
chanAsString (ChanString s) = return s
chanAsString (ChanBSL s) = return . bsl2str $ s
chanAsString (ChanHandle h) = hGetContents h

chanAsBSL :: Channel -> IO BSL.ByteString
chanAsBSL (ChanString s) = return . str2bsl $ s
chanAsBSL (ChanBSL s) = return s
chanAsBSL (ChanHandle h) = BSL.hGetContents h

chanAsBS :: Channel -> IO BS.ByteString
chanAsBS c = do r <- chanAsBSL c
                let contents = BSL.toChunks r
                return . BS.concat $ contents

{- | Writes the Channel to the given Handle. -}
chanToHandle :: Channel -> Handle -> IO ()
chanToHandle (ChanString s) h = hPutStr h s
chanToHandle (ChanBSL s) h = BSL.hPut h s
chanToHandle (ChanHandle srchdl) desthdl = forkIO copier >> return ()
    where copier = do c <- BSL.hGetContents srchdl
                      BSL.hPut desthdl c

class Channelizable a where
    toChannel :: a -> Channel
instance Channelizable String where
    toChannel = ChanString
instance Channelizable BSL.ByteString where
    toChannel = ChanBSL
instance Channelizable Handle where
    toChannel = ChanHandle
instance Channelizable BS.ByteString where
    toChannel bs = ChanBSL . BSL.fromChunks $ [bs]


                    
str2bsl :: String -> BSL.ByteString
str2bsl = BSLC.pack

bsl2str :: BSL.ByteString -> String
bsl2str = BSLC.unpack

