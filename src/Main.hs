{-# LANGUAGE RankNTypes, FlexibleContexts, OverloadedStrings, DeriveAnyClass, ApplicativeDo, DeriveFunctor #-}

module Main where

import Control.Monad
import Control.Applicative
import Conduit
import Control.Concurrent.Async (concurrently)
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
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar


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

type Trans = String

type Message = (Either Trans Trans)

data Parser a = Parser { parse :: Message -> Session a } deriving (Functor, Applicative, Monad)


instance Alternative Parser where
    empty = undefined
    (<|>) = undefined

instance MonadPlus Parser where
    mzero = undefined
    mplus = undefined


record :: Parser Message
record = do
    transmitd "SI_REC_START"
    recvd "MAIN_ST"
    transmitu "SI_RECORDING"


transmitd :: Trans -> Parser Message
transmitd s = do
    recvu s 
    sendd s

transmitu :: Trans -> Parser Message
transmitu s = do
    recvd s 
    sendu s   


recvu :: Trans -> Parser Message
recvu s = Parser $ trans 
    where 
        trans ((Just s'), Nothing) = do 
            guard (s' == s)
            return $ mzero-- Recvu s

recvd :: Trans -> Parser Message 
recvd s = undefined

sendu :: Trans -> Parser Message
sendu s = undefined

sendd :: Trans -> Parser Message
sendd s = undefined


main :: IO ()
main = do
  args <- getArgs
  start args 
  where 
    start ("u":_) = upper
    start ("d":_) = double
    start ("t":p:_) = telnet (read p)
    start _ = print "Usage postream [u|d|t]"

