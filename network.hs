module Network where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as C
import qualified Control.Exception as E
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import System.Timeout
import System.Posix.Types
import Data.Maybe
import Data.Serialize
import Data.Serialize.Put
import GHC.IO.Handle.FD
import System.IO

import Chess

data SyncRole = First | Second deriving (Show)

getSyncRole :: Get SyncRole
getSyncRole = do
  w <- getWord8
  case w of
    0 -> return First
    1 -> return Second
    _ -> fail $ "invalid SyncRole value: '" ++ show w ++ "'"

instance Serialize SyncRole where
  put First = putWord8 $ toEnum 0
  put Second = putWord8 $ toEnum 1
  get = getSyncRole

_ACC = B.singleton 1 :: B.ByteString
_NOACC = B.singleton 0 :: B.ByteString

sendRole :: Socket -> SyncRole -> IO ()
sendRole sock role = do
  sendAll sock $ encode role
  putStrLn "waiting for role ACC"
  acc <- recv sock 1
  case acc of
    _ | acc == _ACC -> return ()
      | acc == _NOACC -> sendRole sock role

acceptRole :: Socket -> IO SyncRole
acceptRole sock = do
  putStrLn $ "waiting for role..."
  msg <- recv sock 1
  if not . B.null $ msg
    then
    case decode msg of
      Left s -> do
        putStrLn $ s ++ "acceptRole: NOACC, trying again"
        sendAll sock _NOACC
        acceptRole sock
      Right r -> do
        sendAll sock _ACC
        return r
    else do
    threadDelay 100000
    acceptRole sock

initCon :: HostName -> ServiceName -> IO Socket
initCon host port = withSocketsDo $ do
  timed <- newChan
  timeoutId <- forkIO $ timeout' 3000000 timed
  connId <- forkOS $ tryConnect timed
  msock <- readChan timed
  case msock of
    Nothing -> do
      putStrLn "Timed out, killing conn sock"
      killThread connId
      waitForOpp host port
    Just sock -> do
      killThread timeoutId
      putStrLn $ "Connected to " ++ host ++ " on port " ++ port
      sendRole sock First
      return sock
  where
    timeout' delay chan = do
      threadDelay delay
      writeChan chan Nothing
    resolve = do
      let hints = defaultHints
            { addrFamily = AF_INET
            , addrSocketType = Stream
            }
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    tryConnect chan = do
        addr <- resolve
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        putStrLn $ "Trying to connect to " ++ host
        connect sock (addrAddress addr) `E.catch` notifyFail chan
        withFdSocket sock (threadWaitWrite . Fd)
        writeChan chan (Just sock)
    notifyFail :: Chan (Maybe Socket) -> E.SomeException -> IO ()
    notifyFail chan _ = do
      writeChan chan Nothing

waitForOpp :: HostName -> ServiceName -> IO Socket
waitForOpp remote port = withSocketsDo $ do
  addr <- resolve
  servSock <- open addr
  putStrLn $ "Opened server on port " ++ port ++ ", waiting for opponent..."
  checkIp servSock
  where
    resolve = do
      let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      head <$> getAddrInfo (Just hints) Nothing (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock $ setCloseOnExecIfNeeded
      bind sock (addrAddress addr)
      listen sock 4
      return sock
    checkIp sock = do
      (conn, _peer) <- accept sock
      myAddr <- getSocketName conn
      if eqAddr _peer myAddr
        then do
        close sock
        (Just oppAddr, _) <- getNameInfo [] True False _peer
        putStrLn $ "Connected to opponent at " ++ show oppAddr
        return conn
        else do
        close conn
        checkIp sock
    eqAddr a1 a2 = True

syncStart :: Socket -> IO Color
syncStart sock = do
  role <- acceptRole sock
  putStrLn $ "accepted role: " ++ show role
  case role of
    First -> sendRole sock Second
    _ -> return ()
    
  return White

main = do
  sock <- initCon "localhost" "30001"
  syncStart sock

