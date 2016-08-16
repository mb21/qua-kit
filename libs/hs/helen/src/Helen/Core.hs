-----------------------------------------------------------------------------
-- |
-- Module      :  Helen.Core
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Helen.Core
  ( Helen (..), program, initHelen
  ) where

import qualified Data.Aeson as JSON
--import Data.Text (Text)
import qualified Data.Text as Text
import Data.Conduit
import Data.Unique
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap

import Luci.Messages
import Luci.Connect
import Luci.Connect.Base
import System.Mem.Weak
import qualified Control.Monad.STM as STM
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Concurrent.STM.TChan as STM
import           Control.Concurrent (forkIO)
import           Control.Monad.Trans.Control (liftBaseWith)
import           Control.Monad.Trans.Class
import           Control.Monad (void, forever)
import qualified Data.Conduit.Network as Network
import qualified Data.ByteString.Lazy.Char8 as LazyBSC


-- | Represent a connected client
newtype ClientId = ClientId Unique
  deriving (Eq,Ord, Hashable)

type Client = Message -> LuciProgram Helen ()

-- | The program state type
data Helen = Helen
  { msgChannel       :: !(STM.TChan (ClientId, Message))
  , sendMessage      :: !(ClientId -> Message -> LuciProgram Helen ())
    -- ^ Send a message to a given client (by client id)
  , registerClient   :: !(Client -> LuciProgram Helen (ClientId, LuciProgram Helen ()))
    -- ^ Register a send-message callback;
    --   Returns own cliendId and an unregister callback
  }

-- | Initialize state
initHelen :: IO Helen
initHelen = do
  ch <- STM.newTChanIO
  clientStore <- STM.newTMVarIO (HashMap.empty :: HashMap.HashMap ClientId Client)
  return Helen
    { msgChannel = ch
    , sendMessage = \cId msg -> do
         mf <- fmap (HashMap.lookup cId) . liftIO . STM.atomically $ STM.readTMVar clientStore
         case mf of
           Nothing -> return ()
           Just f -> f msg
    , registerClient = \putMsg -> liftIO $ do
         cId <- ClientId <$> newUnique
         STM.atomically $ do
             hm <- STM.takeTMVar clientStore
             STM.putTMVar clientStore $ HashMap.insert cId putMsg hm
         return ( cId
                , liftIO . STM.atomically $
                    STM.takeTMVar clientStore >>= STM.putTMVar clientStore . HashMap.delete cId
                )
    }

-- | Run main program
program :: Int -- ^ Port
        -> LuciProgram Helen ()
program port = do
  -- Run all connections in a separate thread. Warning! Helen state is not shared!
  _ <- liftBaseWith $ \run -> forkIO . void . run $ helenChannels port
  -- Now process message queue
  ch <- msgChannel <$> get
  forever $ (liftIO . STM.atomically $ STM.readTChan ch) >>= processMessage



processMessage :: (ClientId, Message)
               -> LuciProgram Helen ()
processMessage (clientId, msg) = do
  helen <- get
  -- return message for now
  sendMessage helen clientId msg
  return ()



-- | Start a TCP server to listen to connections;
--   Each time message comes, it registered together with callback in a global message channel.
helenChannels :: Int                  -- ^ Port
              -> LuciProgram Helen ()
helenChannels port = Network.runGeneralTCPServer connSettings $ helenChannels'
  where
    connSettings = Network.serverSettings port "*4"


-- | Use existing connection to run a message-processing conduit
helenChannels' :: Network.AppData
               -> LuciProgram Helen ()
helenChannels' appData = do
    -- here we store messages to be sent back to client
    sendQueue <- liftIO . STM.atomically $ STM.newTChan
    -- weak reference based on channel state
    weakSink <- liftIO $ mkWeak sendQueue (liftIO . STM.atomically . STM.writeTChan sendQueue . Just . makeMessage) Nothing
    let -- put message to sendback channel
        putMsg msg = msg `seq` do
          msending <- liftIO $ deRefWeak weakSink
          case msending of
            Nothing -> return ()
            Just f  -> f msg
    (clientId, unregister) <- get >>= flip registerClient putMsg
    let -- define a source of data to send to client
        source = do
          mval <- liftIO . STM.atomically $ STM.readTChan sendQueue
          case mval of
            Nothing -> lift $ unregister
            Just v  -> yield v >> source
        -- main message parsing
        sink = do
          rawproc <- await
          case rawproc of
            -- do some finalization actions, e.g. notify Helen that client is disconnected
            Nothing -> do
              liftIO . STM.atomically $ STM.unGetTChan sendQueue Nothing
            -- do actual message processing
            Just (Processing m@(h,_)) -> do
              case parseMessage m of
                  -- if high-level parsing fails, notify client and log warning
                  JSON.Error err -> do
                    let msgerr = Text.pack $ "Unexpected message: " ++ err
                                         ++ ". Received header: " ++ LazyBSC.unpack (JSON.encode h)
                    logWarnN msgerr
                    liftIO . STM.atomically . STM.writeTChan sendQueue . Just . makeMessage $ MsgError msgerr
                  -- If all is good, put message into Helen's msgChannel
                  JSON.Success msg -> do
                    helen <- get
                    liftIO . STM.atomically $ STM.writeTChan (msgChannel helen) (clientId, msg)
              sink
            -- send data back to client if it was early-processed
            Just (ProcessedData d) ->  do
                lift $ yield d $$ outgoing
                sink
            -- Notify about protocol errors
            Just (ProcessingError e) -> logWarnN (Text.pack (show (e :: LuciError Text.Text))) >> sink

    -- asynchroniously send messages
    _ <- liftBaseWith $ \run -> forkIO . void . run $ source $$ writeMessages =$= outgoing
    -- receive messages in current thread (another thread per connection anyway)
    runConduit $ incoming
              =&= parseMsgsPanicCatching
              =&= panicResponseConduit
              =$= sink
  where
    outgoing = Network.appSink appData
    incoming = mapOutput Processing $ Network.appSource appData



