{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Index.Tests
-- Copyright   :  (c) 2010-2015 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Index.Tests (
    prop_lookup,
    prop_toList,
    prop_valid,
    prop_serialise_deserialise,
    prop_serialiseSize,
#ifdef MIN_VERSION_bytestring_handle
    prop_index_matches_tar,
#endif
    prop_finalise_unfinalise,
  ) where

import Codec.Archive.Tar.Index.Internal
import Codec.Archive.Tar.Types as Tar
import qualified Codec.Archive.Tar.Index.IntTrie as IntTrie
import qualified Codec.Archive.Tar.Index.IntTrie.Tests as IntTrie
import qualified Codec.Archive.Tar.Index.StringTable as StringTable
import qualified Codec.Archive.Tar.Index.StringTable.Tests as StringTable

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BS.Char8
import qualified Data.ByteString.Lazy   as LBS
import Data.Int
#if (MIN_VERSION_base(4,5,0))
import Data.Monoid ((<>))
#endif
import qualified System.FilePath.Posix as FilePath
import System.IO

import Prelude hiding (lookup)
import qualified Prelude
import Test.QuickCheck
import Test.QuickCheck.Property (ioProperty)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import Data.List (nub, sort, sortBy, stripPrefix, isPrefixOf)
import Data.Maybe
import Data.Function (on)
import Control.Exception (SomeException, try, throwIO)
import Codec.Archive.Tar.Write          as Tar

#ifdef MIN_VERSION_bytestring_handle
import qualified Data.ByteString.Handle as HBS
#endif

-- Not quite the properties of a finite mapping because we also have lookups
-- that result in completions.

prop_lookup :: ValidPaths -> NonEmptyFilePath -> Bool
prop_lookup (ValidPaths paths) (NonEmptyFilePath p) =
  case (lookup index p, Prelude.lookup p paths) of
    (Nothing,                    Nothing)          -> True
    (Just (TarFileEntry offset), Just (_,offset')) -> offset == offset'
    (Just (TarDir entries),      Nothing)          -> sort (nub (map fst entries))
                                                   == sort (nub completions)
    _                                              -> False
  where
    index       = construct paths
    completions = [ head (FilePath.splitDirectories completion)
                  | (path,_) <- paths
                  , completion <- maybeToList $ stripPrefix (p ++ "/") path ]

prop_toList :: ValidPaths -> Bool
prop_toList (ValidPaths paths) =
    sort (toList index)
 == sort [ (path, off) | (path, (_sz, off)) <- paths ]
  where
    index = construct paths

prop_valid :: ValidPaths -> Bool
prop_valid (ValidPaths paths)
  | not $ StringTable.prop_valid   pathbits = error "TarIndex: bad string table"
  | not $ IntTrie.prop_lookup      intpaths = error "TarIndex: bad int trie"
  | not $ IntTrie.prop_completions intpaths = error "TarIndex: bad int trie"
  | not $ prop'                             = error "TarIndex: bad prop"
  | otherwise                               = True

  where
    index@(TarIndex pathTable _ _) = construct paths

    pathbits = concatMap (map BS.Char8.pack . FilePath.splitDirectories . fst)
                         paths
    intpaths = [ (cids, offset)
               | (path, (_size, offset)) <- paths
               , let Just cids = toComponentIds pathTable path ]
    prop' = flip all paths $ \(file, (_size, offset)) ->
      case lookup index file of
        Just (TarFileEntry offset') -> offset' == offset
        _                           -> False

prop_serialise_deserialise :: ValidPaths -> Bool
prop_serialise_deserialise (ValidPaths paths) =
    Just (index, BS.empty) == (deserialise . serialise) index
  where
    index = construct paths

prop_serialiseSize :: ValidPaths -> Bool
prop_serialiseSize (ValidPaths paths) =
    case (LBS.toChunks . serialiseLBS) index of
      [c1] -> BS.length c1 == serialiseSize index
      _    -> False
  where
    index = construct paths

newtype NonEmptyFilePath = NonEmptyFilePath FilePath deriving Show

instance Arbitrary NonEmptyFilePath where
  arbitrary = NonEmptyFilePath . FilePath.joinPath
                <$> listOf1 (elements ["a", "b", "c", "d"])

newtype ValidPaths = ValidPaths [(FilePath, (Int64, TarEntryOffset))] deriving Show

instance Arbitrary ValidPaths where
  arbitrary = do
      paths <- makeNoPrefix <$> listOf arbitraryPath
      sizes <- vectorOf (length paths) (getNonNegative <$> arbitrary)
      let offsets = scanl (\o sz -> o + 1 + blocks sz) 0 sizes
      return (ValidPaths (zip paths (zip sizes offsets)))
    where
      arbitraryPath   = FilePath.joinPath
                         <$> listOf1 (elements ["a", "b", "c", "d"])
      makeNoPrefix [] = []
      makeNoPrefix (k:ks)
        | all (not . isPrefixOfOther k) ks
                     = k : makeNoPrefix ks
        | otherwise  =     makeNoPrefix ks

      isPrefixOfOther a b = a `isPrefixOf` b || b `isPrefixOf` a

      blocks :: Int64 -> TarEntryOffset
      blocks size = fromIntegral (1 + ((size - 1) `div` 512))

-- Helper for bulk construction.
construct :: [(FilePath, (Int64, TarEntryOffset))] -> TarIndex
construct =
    either (\_ -> undefined) id
  . build
  . foldr (\(path, (size, _off)) es -> Next (testEntry path size) es) Done

example0 :: Entries ()
example0 =
         testEntry "foo-1.0/foo-1.0.cabal" 1500 -- at block 0
  `Next` testEntry "foo-1.0/LICENSE"       2000 -- at block 4
  `Next` testEntry "foo-1.0/Data/Foo.hs"   1000 -- at block 9
  `Next` Done

example1 :: Entries ()
example1 =
  Next (testEntry "./" 1500) Done <> example0

testEntry :: FilePath -> Int64 -> Entry
testEntry name size = simpleEntry path (NormalFile mempty size)
  where
    Right path = toTarPath False name

-- | Simple tar archive containing regular files only
data SimpleTarArchive = SimpleTarArchive {
    simpleTarEntries :: Tar.Entries ()
  , simpleTarRaw     :: [(FilePath, LBS.ByteString)]
  , simpleTarBS      :: LBS.ByteString
  }

instance Show SimpleTarArchive where
  show = show . simpleTarRaw

#ifdef MIN_VERSION_bytestring_handle
prop_index_matches_tar :: SimpleTarArchive -> Property
prop_index_matches_tar sta =
    ioProperty (try go >>= either (\e -> throwIO (e :: SomeException))
                                  (\_ -> return True))
  where
    go :: IO ()
    go = do
      h <- HBS.readHandle True (simpleTarBS sta)
      goEntries h 0 (simpleTarEntries sta)

    goEntries :: Handle -> TarEntryOffset -> Tar.Entries () -> IO ()
    goEntries _ _ Tar.Done =
      return ()
    goEntries _ _ (Tar.Fail _) =
      throwIO (userError "Fail entry in SimpleTarArchive")
    goEntries h offset (Tar.Next e es) = do
      goEntry h offset e
      goEntries h (nextEntryOffset e offset) es

    goEntry :: Handle -> TarEntryOffset -> Tar.Entry -> IO ()
    goEntry h offset e = do
      e' <- hReadEntry h offset
      case (Tar.entryContent e, Tar.entryContent e') of
        (Tar.NormalFile bs sz, Tar.NormalFile bs' sz') ->
          unless (sz == sz' && bs == bs') $
            throwIO $ userError "Entry mismatch"
        _otherwise ->
          throwIO $ userError "unexpected entry types"
#endif

instance Arbitrary SimpleTarArchive where
  arbitrary = do
      numEntries <- sized $ \n -> choose (0, n)
      rawEntries <- mkRaw numEntries
      let entries = mkList rawEntries
      return SimpleTarArchive {
          simpleTarEntries = mkEntries entries
        , simpleTarRaw     = rawEntries
        , simpleTarBS      = Tar.write entries
        }
    where
      mkRaw :: Int -> Gen [(FilePath, LBS.ByteString)]
      mkRaw 0 = return []
      mkRaw n = do
         -- Pick a size around 0, 1, or 2 block boundaries
         sz <- sized $ \n -> elements (take n fileSizes)
         bs <- LBS.pack `fmap` vectorOf sz arbitrary
         es <- mkRaw (n - 1)
         return $ ("file" ++ show n, bs) : es

      mkList :: [(FilePath, LBS.ByteString)] -> [Tar.Entry]
      mkList []            = []
      mkList ((fp, bs):es) = entry : mkList es
        where
          Right path = toTarPath False fp
          entry   = simpleEntry path content
          content = NormalFile bs (LBS.length bs)

      mkEntries :: [Tar.Entry] -> Tar.Entries ()
      mkEntries []     = Tar.Done
      mkEntries (e:es) = Tar.Next e (mkEntries es)

      -- Sizes around 0, 1, and 2 block boundaries
      fileSizes :: [Int]
      fileSizes = [
                           0 ,    1 ,    2
        ,  510 ,  511 ,  512 ,  513 ,  514
        , 1022 , 1023 , 1024 , 1025 , 1026
        ]

-- | 'IndexBuilder' constructed from a 'SimpleIndex'
newtype SimpleIndexBuilder = SimpleIndexBuilder IndexBuilder
  deriving Show

instance Arbitrary SimpleIndexBuilder where
  arbitrary = SimpleIndexBuilder . build' . simpleTarEntries <$> arbitrary
    where
      -- like 'build', but don't finalize
      build' :: Show e => Entries e -> IndexBuilder
      build' = go empty
        where
          go !builder (Next e es) = go (addNextEntry e builder) es
          go !builder  Done       = builder
          go !_       (Fail err)  = error (show err)

prop_finalise_unfinalise :: SimpleIndexBuilder -> Bool
prop_finalise_unfinalise (SimpleIndexBuilder index) =
    unfinalise (finalise index) == index

#if !(MIN_VERSION_base(4,5,0))
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
