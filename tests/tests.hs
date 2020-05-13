{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Data.Default                 (def)
import           System.IO.Temp               (withSystemTempDirectory)

import           Database.RocksDB             (Compression (..), DB, compression,
                                               createIfMissing, defaultOptions, get, open,
                                               put)

import           Test.Hspec                   (describe, hspec, it, shouldReturn)
import           Test.QuickCheck              (Arbitrary (..), UnicodeString (..),
                                               generate)
import Conduit (runConduit, (.|), sinkList)
import Database.RocksDB.Base (range, close)
import Control.Exception (bracket)

initializeDB :: MonadResource m => FilePath -> m DB
initializeDB path =
    open
        path
        defaultOptions
        {createIfMissing = True, compression = NoCompression}

main :: IO ()
main =  hspec $ do

  describe "Basic DB Functionality" $ do
    it "should put items into the database and retrieve them" $  do
      runResourceT $ withSystemTempDirectory "rocksdb" $ \path -> do
        db <- initializeDB path
        put db def "zzz" "zzz"
        get db def "zzz"
      `shouldReturn` (Just "zzz")
      
    it "should put items into the database and return items in a range" $ do
      bracket 
        (withSystemTempDirectory "rocksdb" (\path -> do
          open path defaultOptions{createIfMissing = True, compression = NoCompression})) 
        close
        $ \db -> do 
          put db def "key1" "value1"
          put db def "key2" "value2"
          put db def "key3" "value3"
          source <- range db "key1" "key4" 
          runConduit $ source .| sinkList 
      `shouldReturn` [("key1","value1"),("key2","value2"),("key3","value3")]
      
