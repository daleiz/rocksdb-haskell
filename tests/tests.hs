{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Data.Default                 (def)
import           System.IO.Temp               (withSystemTempDirectory)

import           Database.RocksDB             (Compression (..), DB,
                                               MergeOperator (..), close,
                                               compression, createIfMissing,
                                               defaultOptions, get, merge,
                                               mergeOperator, open, put, range)

import           Conduit                      (runConduit, sinkList, (.|))
import           Control.Exception            (bracket)
import           Database.RocksDB.Base        (binaryToBS)
import           Test.Hspec                   (describe, hspec, it,
                                               shouldReturn)
import           Test.QuickCheck              (Arbitrary (..),
                                               UnicodeString (..), generate)

import           Data.Word

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

    it "should put items into the database and return items in a range" $  do
      runResourceT $ withSystemTempDirectory "rocksdb" $ \path -> do
        db <- initializeDB path
        put db def "key1" "value1"
        put db def "key2" "value2"
        put db def "key3" "value3"
        source <- range db "key1" "key4"
        runConduit $ source .| sinkList
      `shouldReturn` [("key1","value1"),("key2","value2"),("key3","value3")]

  describe "Merge Operator" $ do
    it "uint64add merge operator test0" $  do
      runResourceT $ withSystemTempDirectory "rocksdb" $ \path -> do
        db <- open
            path
            defaultOptions
            {createIfMissing = True, compression = NoCompression, mergeOperator = UInt64Add}
        put db def "key1" (binaryToBS (0 :: Word64))
        merge db def "key1" (binaryToBS (1 :: Word64))
        get db def "key1"
      `shouldReturn` (Just (binaryToBS (1 :: Word64)))
      
    it "uint64add merge operator test1" $  do
      runResourceT $ withSystemTempDirectory "rocksdb" $ \path -> do
        db <- open
            path
            defaultOptions
            {createIfMissing = True, compression = NoCompression, mergeOperator = UInt64Add}
        put db def "key1" (binaryToBS (1 :: Word64))
        merge db def "key1" (binaryToBS (1024 :: Word64))
        get db def "key1"
      `shouldReturn` (Just (binaryToBS (1025 :: Word64)))

