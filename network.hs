module Network where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Control.Exception as E
import Control.Concurrent (forkFinally, forkIO)
import Control.Monad

main = do
  forkIO $ runTCPServer Nothing "30000" talk
  runTCPClient "127.0.0.1" "30000" $ listen

  where
    listen s = do
      msgToSend <- getLine
      sendAll s $ C.pack msgToSend
      msg <- recv s 1024
      putStrLn $ "received: " ++ C.unpack msg
      listen s
                                        
    talk s = do
      msg <- recv s 1024
      unless (B.null msg) $ do
        sendAll s msg
        talk s

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- head <$> resolve
  E.bracket (con addr) close client
  where
    resolve = do
      let hints = defaultHints { addrSocketType = Stream }
      getAddrInfo (Just hints) (Just host) (Just port)
    con addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock (addrAddress addr)
      return sock

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints
            { addrFamily = AF_INET
            , addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock $ setCloseOnExecIfNeeded
      bind sock (addrAddress addr)
      listen sock 10
      return sock
    loop sock = forever $ do
      (conn, _peer) <- accept sock
      void $ forkFinally (server conn) (const $ gracefulClose conn 1000)


