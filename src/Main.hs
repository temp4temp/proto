{-# LANGUAGE RankNTypes, FlexibleContexts, FlexibleInstances, OverloadedStrings, DeriveAnyClass, ApplicativeDo, DeriveFunctor #-}

module Main where

import Control.Monad
import Control.Applicative
import Conduit
import Control.Concurrent.Async (concurrently)
import Control.Concurrent (forkIO)

import Data.Conduit.TMChan
import Data.Conduit.Network
import Data.Word8 (toUpper)
import Data.ByteString      (pack)

import System.Environment (getArgs)
import System.Exit (exitSuccess)

import qualified Data.ByteString as S
import qualified System.IO as IO


import Control.Monad.Trans.Resource
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar

import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TMChan

import Data.Monoid


--lightloop :: AppData -> 


termC :: (Show i, Eq i, MonadIO m) => i -> Conduit i m i
termC t = do
  rs <- await 
  case rs of
    Nothing -> return ()
    (Just x) -> case x == t of
        True -> do
          liftIO $ exitSuccess
        False -> do
          liftIO $ print x
          yield x
          termC t


upper :: IO ()
upper = do 
  runTCPServer (serverSettings 4000 "*") (\ appData -> 
    appSource appData 
    $$ omapCE toUpper =$ appSink appData)

double :: IO ()
double = do
  runTCPServer (serverSettings 4001 "*") (\ appData -> 
    (appSource appData .| (termC "T\n") .| concatMapCE ( \ w -> pack [w,w])) $$ appSink appData)

telnet :: Int -> IO ()
telnet port = do
  runTCPClient (clientSettings port "localhost") $ \server -> 
    void $ concurrently
      (stdinC $$ appSink server)
      (appSource server $$ stdoutC)

-- Applicative ftp frase
-- Conn start recording -> 

data Session a = 
    Connect String (Command a)
    | Disconnect 
    | Error (Command a) deriving (Functor, Applicative, Monad)
    
data Command a = 
    Recvu a 
    | Recvd a
    | Sendu a 
    | Sendd a
    | Term deriving (Functor, Applicative, Monad)

type Transport = S.ByteString

type Trans = Either Transport Transport

type CurState = String

type Message = (TMChan Transport, TMChan Transport, (TMVar Trans)) -- TMChan (Either Trans Trans)

-- type Parser a = ReaderT Message (StateT CurState IO ) a -- Parser { parse :: Message -> Session a } deriving (Functor, Applicative, Monad)
type Parser a = ReaderT Message IO a -- Parser { parse :: Message -> Session a } deriving (Functor, Applicative, Monad)

type Result = Bool

-- instance (MonadIO m) => Monoid (m (TMVar a)) where
   -- mempty = liftIO newEmptyTMVarIO
--    mappend a b = undefined


{-
instance Alternative Parser where
    empty = undefined
    (<|>) = undefined

instance MonadPlus Parser where
    mzero = undefined
    mplus = undefined
-}


errlog :: Parser Result
errlog = do
    (ab, dv, a) <- ask 
    tod "xxx"
    liftIO $ do
        rs <- atomically $ tryTakeTMVar a
        print $ "skipping >> " ++ (show rs)
        return False

tod :: Transport -> Parser ()
tod s = do
    (_, bd, _) <- ask
    liftIO $ atomically $ writeTMChan bd s
    return ()

toa :: Transport -> Parser ()
toa s = do
    
    return ()    

record :: Parser Result
record = do
    oneof $ Left "startr"
    tod "SI_RECORD"
    waitoneof $ stoprec <|> sirec <|> errlog
    where
        stoprec = do
            oneof $ Left "stopr"
            rs <- waitfor $ Right "siidle"
            toa "done recording"
            return rs
        sirec = do
            oneof $ Right "sirec"
            rs <- waitoneof $ stoprec <|> sirec  <|> errlog
            toa "recording"
            return rs

recvu a = oneof a

oneof :: Trans -> Parser Result
oneof s = do 
    (ab, dv, a) <- ask 
    rs <- liftIO $ atomically $ readTMVar a
    liftIO $ print $ "having " ++ (show rs)
    liftIO $ print $ "looking for " ++ (show s)
    guard ((rs == s))
    liftIO $ print $ "it is" ++ (show rs)
    liftIO $ atomically $ takeTMVar a
    return True

waitfor :: Trans -> Parser Result
waitfor s = do
    loop
    where
        loop = do
            (ab, dv, a) <- ask
            rs <- liftIO $ atomically $ takeTMVar a
            if rs == s
            then do
                liftIO $ print $ "got " ++ (show rs)
                return True
            else do
                liftIO $ print $ "skip " ++ (show rs)
                loop

waitoneof :: Parser Result -> Parser Result
waitoneof p = ReaderT $ \ a -> do
    rs <- (runReaderT p) a
    if rs 
    then return rs
    else runReaderT (waitoneof p) a

recvd :: Trans -> Parser Message 
recvd s = undefined

initToBrockerChan :: TMVar Trans -> IO (TMChan Transport, TMChan Transport)
initToBrockerChan tv = do
    ab <- newTMChanIO
    db <- newTMChanIO
    forkIO $ void $ concurrently
              (sourceTMChan ab $$ (awaitForever ( \ i -> do liftIO (print ">>>in ab");liftIO (atomically $ putTMVar tv (Left i)); return ())))
              (sourceTMChan db $$ (awaitForever ( \ i -> do liftIO (print ">>>in db");liftIO (atomically $ putTMVar tv (Right i)); return ())))
    return (ab, db)

initFromBrockerChan :: IO (TMChan Transport, TMChan Transport)
initFromBrockerChan = do
    ba <- newTMChanIO
    bd <- newTMChanIO
    return (ba, bd)

test :: S.ByteString -> Trans
test s = Left s




main :: IO ()
main = do
    tv <- newTMVarIO $ test "startr"
    (ab, db) <- initToBrockerChan tv
    (ba, bd) <- initFromBrockerChan
    forkIO $ runTCPClient (clientSettings 4001 "localhost") $ \server -> 
                void $ concurrently
                  (stdinC $$  sinkTMChan ab True)
                  (appSource server $$ ((awaitForever ( \ i -> do liftIO (print "+++++");yield i)) .| sinkTMChan db True))

    forkIO $ runTCPClient (clientSettings 4001 "localhost") $ \server -> 
                void $ concurrently
                  (sourceTMChan bd $$ appSink server)
                  (sourceTMChan ba $$ stdoutC)
    runReaderT record (ba, bd, tv)
    return ()

test1 :: (TMChan Transport, TMChan Transport, TMVar Trans) -> IO ()
test1  c@(ba, bd, tv) = do
    void $ concurrently
        (do 
            atomically (putTMVar tv (Right "sirec"))
            atomically (putTMVar tv (Right "sirec"))
            atomically (putTMVar tv (Right "sirec"))
            atomically (putTMVar tv (Right "stopr"))
            atomically (putTMVar tv (Right "stopr"))
            atomically (putTMVar tv (Left "stopr"))
            atomically (putTMVar tv (Right "xxx"))
            atomically (putTMVar tv (Left "xxx"))
            atomically (putTMVar tv (Right "siidle"))
            atomically (putTMVar tv (Right "siidle"))
            atomically (putTMVar tv (Right "siidle")))
        (runReaderT record c)
    print "done test1"
{-
test1a :: IO ()
test1a = do
    tv <- newTMVarIO $ test "startr"
    void $ concurrently
        (do 
            atomically (putTMVar tv (Right "sirec"))
            atomically (putTMVar tv (Right "sirec"))
            atomically (putTMVar tv (Right "sirec"))
            atomically (putTMVar tv (Right "stopr"))
            atomically (putTMVar tv (Left "stopr")))
        (runReaderT record tv)
    print "done test1"


test2 :: IO ()
test2 = do
    tv <- newTMVarIO $ test "startr"
    void $ concurrently
        (do 
            atomically (putTMVar tv (Right "sirec"))
            atomically (putTMVar tv (Right "sirec"))
            atomically (putTMVar tv (Right "sirec"))
            atomically (putTMVar tv (Left "stopr"))
            atomically (putTMVar tv (Right "siidle")))
        (runReaderT (record <|> errlog) tv)        
    print "done test2"


test2a :: IO ()
test2a = do
    tv <- newTMVarIO $ test "startr"
    void $ concurrently
        (do 
            atomically (putTMVar tv (Right "sirec"))
            atomically (putTMVar tv (Right "sirec"))
            atomically (putTMVar tv (Right "sirec"))
            atomically (putTMVar tv (Left "stopr")))
        (runReaderT (record <|> errlog) tv)        
    print "done test2"

main1 :: IO ()
main1 = do
  args <- getArgs
  start args 
  where 
    start ("u":_) = upper
    start ("d":_) = double
    start ("t":p:_) = telnet (read p)
    start _ = print "Usage postream [u|d|t]"

-}