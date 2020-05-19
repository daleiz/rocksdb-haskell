-- |
-- Module      : Database.RocksDB.Types
-- Copyright   : (c) 2012-2013 The leveldb-haskell Authors
--               (c) 2014 The rocksdb-haskell Authors
-- License     : BSD3
-- Maintainer  : mail@agrafix.net
-- Stability   : experimental
-- Portability : non-portable
--

module Database.RocksDB.Types
    ( BatchOp (..)
    , BloomFilter (..)
    , Comparator (..)
    , Compression (..)
    , FilterPolicy (..)
    , Options (..)
    , Property (..)
    , ReadOptions (..)
    , Snapshot (..)
    , WriteBatch
    , WriteOptions (..)
    , MergeOperator(..)

    , defaultOptions
    , defaultReadOptions
    , defaultWriteOptions
    )
where

import           Data.ByteString    (ByteString)
import           Data.Default
import           Foreign

import           Database.RocksDB.C

-- | Snapshot handle
newtype Snapshot = Snapshot SnapshotPtr deriving (Eq)

-- | Compression setting
data Compression = NoCompression
    | SnappyCompression
    | ZlibCompression
    deriving (Eq, Show)

-- | User-defined comparator
newtype Comparator = Comparator (ByteString -> ByteString -> Ordering)

-- | User-defined filter policy
data FilterPolicy = FilterPolicy
    { fpName       :: String
    , createFilter :: [ByteString] -> ByteString
    , keyMayMatch  :: ByteString -> ByteString -> Bool
    }

-- | Represents the built-in Bloom Filter
newtype BloomFilter = BloomFilter FilterPolicyPtr

-- | MergeOperator setting
-- | built-in MergeOperator
data MergeOperator = NoMerge
    | UInt64Add
    deriving (Eq, Show)

-- | Options when opening a database
data Options = Options
    { comparator      :: !(Maybe Comparator)
    -- ^ Comparator used to defined the order of keys in the table.
    , compression     :: !Compression
    -- ^ Compress blocks using the specified compression algorithm.
    , createIfMissing :: !Bool
    -- ^ If true, the database will be created if it is missing.
    , errorIfExists   :: !Bool
    -- ^ It true, an error is raised if the database already exists.
    , maxOpenFiles    :: !Int
    -- ^ Number of open files that can be used by the DB.
    , paranoidChecks  :: !Bool
    -- ^ If true, the implementation will do aggressive checking of the data
    , writeBufferSize :: !Int
    -- ^ Amount of data to build up in memory (backed by an unsorted log on
    , mergeOperator   :: !MergeOperator
    -- ^ MergeOperator
    }

defaultOptions :: Options
defaultOptions = Options
    { comparator           = Nothing
    , compression          = SnappyCompression
    , createIfMissing      = False
    , errorIfExists        = False
    , maxOpenFiles         = 1000
    , paranoidChecks       = False
    , writeBufferSize      = 4 `shift` 20
    , mergeOperator        = NoMerge
    }

instance Default Options where
    def = defaultOptions

-- | Options for write operations
data WriteOptions = WriteOptions
    { sync :: !Bool
    -- ^ If true, the write will be flushed from the operating system buffer
    }
    deriving (Eq, Show)

defaultWriteOptions :: WriteOptions
defaultWriteOptions = WriteOptions { sync = False }

instance Default WriteOptions where
    def = defaultWriteOptions

-- | Options for read operations
data ReadOptions = ReadOptions
    { verifyCheckSums :: !Bool
    -- ^ If true, all data read from underlying storage will be verified
    , fillCache       :: !Bool
    -- ^ Should the data read for this iteration be cached in memory? Callers
    , useSnapshot     :: !(Maybe Snapshot)
    -- ^ If 'Just', read as of the supplied snapshot (which must belong to the
    }
    deriving (Eq)

defaultReadOptions :: ReadOptions
defaultReadOptions = ReadOptions
    { verifyCheckSums = False
    , fillCache       = True
    , useSnapshot     = Nothing
    }

instance Default ReadOptions where
    def = defaultReadOptions

type WriteBatch = [BatchOp]

-- | Batch operation
data BatchOp = Put ByteString ByteString
    | Del ByteString
    deriving (Eq, Show)

-- | Properties exposed by RocksDB
data Property = NumFilesAtLevel Int
    | Stats
    | SSTables
    deriving (Eq, Show)
