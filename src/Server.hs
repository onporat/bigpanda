{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Server where

import           Control.Applicative        ((*>))
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.Async   (Concurrently (..))
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Conduit
import qualified Data.Conduit.Binary        as CB
import qualified Data.Conduit.List          as CL
import           Data.Conduit.Process       hiding (createPipe)
import qualified Data.Conduit.Text          as CT
import           Data.IORef
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Monoid
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           GHC.Generics
import           Snap
import           System.Posix.IO            (closeFd, createPipe, fdToHandle)

data Event
   = Event { event_type      :: !Text
           , event_data      :: !Text
           , event_timestamp :: Int
           } deriving (Show,Generic)

instance FromJSON Event where
 parseJSON (Object v) =
    Event <$> v .: "event_type"
          <*> v .: "data"
          <*> v .: "timestamp"
 parseJSON _ = mempty

data State
   = State { _state_typeCount
           , _state_wordCount :: Map Text Int
           } deriving (Show,Generic)

makeLenses ''State
instance ToJSON State

emptyState :: State
emptyState = State
  { _state_typeCount = mempty
  , _state_wordCount = mempty
  }

countType :: Event -> State -> State
countType e = state_typeCount %~ addWordCount (event_type e)

countWord :: Event -> State -> State
countWord e = state_wordCount %~ addWordCount (event_data e)

addWordCount :: Text -> Map Text Int -> Map Text Int
addWordCount w = Map.unionWith (+) (Map.singleton w 1)

atomicModifyIORef_' :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_' r f = atomicModifyIORef' r $ \a -> (f a, ())

startGenerator :: IORef State -> IO ()
startGenerator sRef = do
    state <- liftIO $ readIORef sRef

    ((input, close), stdout, stderr, cph)
        <- streamingProcess (proc "./bin/generator-linux-amd64" [])

    -- stub to satisfy the compiler
    let inputCloseStub = Concurrently $ do
            yield "Feeding standard input" $$ input
            close

    let stderrSink  = stderr $$ CL.mapM_
          (\bs -> putStrLn $ "from stderr: " ++ show bs)

    let parseJson   = CL.mapMaybe
          (decode . LB.fromStrict .  B.dropWhile ('{' /=))

    let counterSink = CL.mapM_ $ \evnt -> do
          atomicModifyIORef_' sRef $ (countType evnt) . (countWord evnt)
          return ()

    let eventProcessingPipeline =
             stdout
          $$ CB.lines
          =$ parseJson
          =$ counterSink

    forkIO $ runConcurrently
           $ Concurrently eventProcessingPipeline
          *> Concurrently stderrSink

    return ()


site :: IORef State -> Snap ()
site sRef = do
    state <- liftIO $ readIORef sRef
    writeBS $ LB.toStrict $ encode state


main :: IO ()
main = do
    sRef <- newIORef emptyState
    startGenerator sRef
    quickHttpServe $ site sRef
