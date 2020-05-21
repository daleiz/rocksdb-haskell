{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
  ( (.|),
    mapC,
    mapM_C,
    runConduit,
    sinkList,
  )
import Control.Exception (bracket, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource
  ( MonadResource,
    allocate,
    runResourceT,
  )
import Data.Default (def)
import Data.Word
import Database.RocksDB
  ( Compression (..),
    DB,
    MergeOperator (..),
    close,
    compression,
    createIfMissing,
    defaultOptions,
    get,
    merge,
    mergeOperator,
    open,
    put,
    range,
  )
import Database.RocksDB.Base (binaryToBS)
import System.IO.Temp (createTempDirectory)
import Test.Hspec
  ( describe,
    hspec,
    it,
    shouldReturn,
  )
import Test.QuickCheck
  ( Arbitrary (..),
    UnicodeString (..),
    generate,
  )

initializeDB :: MonadIO m => FilePath -> m DB
initializeDB path =
  open
    path
    defaultOptions
      { createIfMissing = True,
        compression = NoCompression,
        mergeOperator = UInt64Add
      }

main :: IO ()
main = hspec $ do 
  describe "Basic DB Functionality" $
    do
      it "should put items into the database and retrieve them" $
        runResourceT
          ( do
              (_, path) <- createTempDirectory Nothing "rocksdb"
              (_, db) <- allocate (initializeDB path) close
              put db def "zzz" "zzz"
              get db def "zzz"
          )
          `shouldReturn` Just "zzz"
      it "should put items into the database and return items in a range" $
        runResourceT
          ( do
              (_, path) <- createTempDirectory Nothing "rocksdb"
              (_, db) <- allocate (initializeDB path) close
              put db def "key1" "value1"
              put db def "key2" "value2"
              put db def "key3" "value3"
              source <- range db "key1" "key3"
              case source of Just s -> runConduit $ s .| sinkList
          )
          `shouldReturn` [ Just ("key1", "value1"),
                           Just ("key2", "value2"),
                           Just ("key3", "value3")
                         ]
                         
  describe "merge operator" $ 
    do
      it "for u64add operator, default value shoule be 0" $
        runResourceT
          ( do
              (_, path) <- createTempDirectory Nothing "rocksdb"
              (_, db) <- allocate (initializeDB path) close
              merge db def "key1" (binaryToBS (1 :: Word64))
              get db def "key1"
          )
          `shouldReturn` Just (binaryToBS (1 :: Word64))
      it "u64add operator should work" $
        runResourceT
          ( do
              (_, path) <- createTempDirectory Nothing "rocksdb"
              (_, db) <- allocate (initializeDB path) close
              put db def "key1" (binaryToBS (4 :: Word64))
              merge db def "key1" (binaryToBS (1 :: Word64))
              get db def "key1"
          )
          `shouldReturn` Just (binaryToBS (5 :: Word64))
