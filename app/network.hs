{-# LANGUAGE MultiParamTypeClasses #-}
module Network where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteArray as A
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
import Crypto.Random
import Crypto.Hash
import Crypto.Error
import Data.Int
import Data.Word
import Data.Function
import Data.Bits
import Data.List
import Crypto.Data.Padding

import Chess
import Serialize

data SyncRole = Bob | Alice deriving (Enum, Show)

instance Serialize SyncRole where
  put = putEnum8
  get = getEnum8

data NetworkErrorCritical = UntrustedHostError deriving (Show)

instance CriticalError NetworkErrorCritical where
  trigger = show

data NetworkError = InvalidMoveError

instance MoveError NetworkError NetworkErrorCritical where
  handleError InvalidMoveError = Left UntrustedHostError

data NetworkPlayer = NetworkP Socket Color

instance Player NetworkPlayer NetworkError NetworkErrorCritical where
  color (NetworkP _ color) = color
  getMove (NetworkP sock _) = getMoveNetwork sock

_ACC = B.singleton 1 :: B.ByteString
_NOACC = B.singleton 0 :: B.ByteString

_SEED_SIZE = 16 :: Int

sendRole :: Socket -> SyncRole -> IO ()
sendRole sock role = do
  sendAll sock $ encode role
  acc <- recv sock 1
  case acc of
    _ | acc == _ACC -> return ()
      | acc == _NOACC -> sendRole sock role

acceptRole :: Socket -> IO SyncRole
acceptRole sock = do
  msg <- recv sock 1
  if not . B.null $ msg
    then
    case decode msg of
      Left s -> do
        putStrLn $ intercalate "; " . lines $ (s ++ "acceptRole: sending NOACC, trying again")
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
  addr <- resolve
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  timeoutId <- forkIO $ timeout' 3000000 timed
  connId <- forkOS $ tryConnect sock addr timed
  msock <- readChan timed
  case msock of
    Nothing -> do
      putStrLn "Timed out, closing connect socket"
      close sock
      killThread connId
      waitForOpp host port
    Just sock -> do
      killThread timeoutId
      putStrLn $ "Connected to " ++ host ++ " on port " ++ port
      sendRole sock Bob
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
    tryConnect sock addr chan = do
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

unlessACC :: Socket -> (Socket -> IO a) -> IO ()
unlessACC sock handler = do
  acc <- recv sock 1
  case acc of
    _ | acc == _ACC -> do
          putStrLn "Got ACC"
          return ()
      | otherwise -> do
          putStrLn "Got NOACC"
          void $ handler sock
 
combineSeeds :: A.ByteArrayAccess ba => ba -> ba -> Seed
combineSeeds sa sb =
  let mrab = seedFromBinary $ pad (ZERO 40) $ B.pack $ (zipWith xor `on` A.unpack) sa sb
  in case mrab of
       CryptoPassed rab -> rab
       CryptoFailed err -> error $ "error: " ++ show err

randomColor :: Seed -> SyncRole -> Color
randomColor seed role =
  let roleFlip = case role of
        Bob -> xor 1
        Alice -> id
      (bytes, _) = randomBytesGenerate 1 $ drgNewSeed seed
      rab = B.head bytes
  in toEnum . fromEnum $ (roleFlip rab .&. 1)

  
-- |Negotiates a random number with an untrusted party, "Alice"
getRandomBob :: Socket -> IO Seed
getRandomBob sock = do
  rb <- getRandomBytes _SEED_SIZE :: IO B.ByteString
  let commit = hashWith SHA3_256 rb
  sendAll sock $ B.pack . A.unpack $ commit

  ra <- recv sock _SEED_SIZE
  sendAll sock rb

  unlessACC sock getRandomBob

  return $ combineSeeds ra rb

-- |Negotiates a random number with an untrusted party, "Bob"
getRandomAlice :: Socket -> IO Seed
getRandomAlice sock = do
  commit <- recv sock 32

  ra <- getRandomBytes _SEED_SIZE :: IO B.ByteString
  sendAll sock ra

  rb <- recv sock _SEED_SIZE
  let verify = hashWith SHA3_256 rb
  case (B.pack . A.unpack) verify == commit of
    True -> sendAll sock _ACC
    False -> do
      sendAll sock _NOACC
      void $ getRandomAlice sock

  return $ combineSeeds ra rb

syncStart :: Socket -> IO (Color, Color)
syncStart sock = do
  role <- acceptRole sock
  putStrLn $ "Accepted role: " ++ show role
  case role of
    Bob -> do
      sendRole sock Alice
      seed <- getRandomBob sock
      return $ (randomColor seed Bob, randomColor seed Alice)
    _ -> do
      seed <- getRandomAlice sock
      return $ (randomColor seed Alice, randomColor seed Bob)

getMoveNetwork :: Socket -> Game -> IO (Either NetworkError Move)
getMoveNetwork sock game@(Game brd lastMv ps@(WPlayer p:_)) = do
  case lastMv of
    Nothing -> return ()
    Just mv -> do
      sendAll sock $ encode lastMv
      unlessACC sock $ (flip getMoveNetwork) game 
      withFdSocket sock (threadWaitRead . Fd)
  recvMv
  where
    recvMv :: IO (Either NetworkError Move)
    recvMv = do
      msg <- recv sock 32
      putStrLn $ "msg: " ++ show (B.unpack msg)
      case decode msg of
        Left e -> do
          putStrLn $ e ++ "Invalid move format received"
          sendAll sock _NOACC
          return $ Left InvalidMoveError
        Right oppMv -> do
          case liftM (validMove brd (color p)) oppMv of
            Nothing -> do
              sendAll sock _NOACC
              putStrLn $ "NOMOVE received"
              recvMv
            Just False -> do
              sendAll sock _NOACC
              putStrLn $ "Invalid move received: '" ++ show (fromJust oppMv) ++ "'"
              return $ Left InvalidMoveError
            Just True -> do
              putStrLn $ "Got move: " ++ show (fromJust oppMv)
              sendAll sock _ACC
              return $ Right (fromJust oppMv)

initN :: HostName -> ServiceName -> IO (Socket, (Color, Color))
initN host port = withSocketsDo $ do
  sock <- initCon host port

  putStrLn $ "Initiating color handshake"
  colrs <- syncStart sock
  putStr "Color handshake succesful, "
  putStrLn $ "color: " ++ show (fst colrs)

  return (sock, colrs)

