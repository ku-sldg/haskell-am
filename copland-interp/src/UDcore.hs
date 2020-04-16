{-# LANGUAGE OverloadedStrings #-}

{- from:  https://hackage.haskell.org/package/network-3.1.1.0/docs/Network-Socket.html#g:18 -}

{-# LANGUAGE CPP #-}

-- | Simple functions to run UnixDomain Socket clients and servers.
module UDcore (
    runUnixDomainClient
  , runUnixDomainServer
  ) where

import System.Directory
import System.IO.Error

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Network.Socket

-- my first attempt
runUnixDomainClient :: String -> (Socket -> IO a) -> IO a
runUnixDomainClient socketPathname client = withSocketsDo $ do
#if MIN_VERSION_network(3,1,1)
    E.bracket (open socketPathname) (\sock -> gracefulClose sock 5000) client
#else
    E.bracket (open socketPathname) close client
#endif
  where
    open pathname = do
        sock <- socket AF_UNIX Stream defaultProtocol
        connect sock (SockAddrUnix pathname)
        return sock

-- | Running a server with an accepted socket identifier (pathname)

maxQueued = 5 :: Int

runUnixDomainServer :: String -> (Socket -> IO a) -> IO a
runUnixDomainServer socketPathname serverAction = withSocketsDo $ do
#if MIN_VERSION_network(3,1,1)
    E.bracket (open socketPathname) (\sock -> gracefulClose sock 5000) loop
#else
    E.bracket (open socketPathname) close loop
#endif
  where
    open pathname = do
      removeIfExists socketPathname
      sock <- socket AF_UNIX Stream defaultProtocol
      bind sock (SockAddrUnix pathname)
      listen sock maxQueued
      return sock
    loop sock = forever $ do
        --error "in server"
        (conn, _peer) <- accept sock
        --error "past accept sock"
#if MIN_VERSION_network(3,1,1)
        --error "3,1,1"
        void $ forkFinally (serverAction conn) (const $ gracefulClose conn 5000)
#else
        --error "not 3,1,1"
        void $ forkFinally (serverAction conn) (const $ close conn)
#endif

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `E.catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = E.throwIO e
