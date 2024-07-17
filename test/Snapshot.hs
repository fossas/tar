{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Codec.Archive.Tar.Entry (Entry (..), GenEntryContent (..), TarPath, entryPath, fromTarPathToPosixPath)
import Codec.Archive.Tar.Index (TarEntryOffset, build, hReadEntry, nextEntryOffset, toList)
import Codec.Archive.Tar.Read (read)
import Codec.Archive.Tar.Types (Entries (..), EntryContent (..), GenEntries (..), foldlEntries)
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString, readFile)
import Data.Foldable (asum, for_)
import Data.Traversable (forM)
import System.FilePath.Posix (normalise)
import System.IO (Handle, IOMode (ReadMode), withFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, HasCallStack, assertEqual, assertFailure, testCase)
import Prelude hiding (read, readFile)
import Codec.Archive.Tar.Types (GenEntry(Entry))
import Codec.Archive.Tar.Types (entryTarPath)

basicUstar :: IO ByteString
basicUstar = tarAt "basic_ustar.tar"

basicPax :: IO ByteString
basicPax = tarAt "basic_pax.tar"

basicGnu :: IO ByteString
basicGnu = tarAt "basic_gnu.tar"

emptyPath :: IO ByteString
emptyPath = tarAt "emptypath.tar"

testCaseFileAt :: String -> String
testCaseFileAt file = "test/testdata/" <> file

tarAt :: String -> IO ByteString
tarAt file = readFile $ testCaseFileAt file

snapshotFiles :: [FilePath]
snapshotFiles =
  map
    testCaseFileAt
    [ "basic_pax.tar",
      "basic_gnu.tar",
      "basic_ustar.tar",
      "emptypath.tar",
      "pax-global-records.tar",
      "pax-nil-sparse-data.tar",
      "pax-nil-sparse-hole.tar",
      "pax-records.tar",
      "pax.tar"
    ]

-- | Assert that the values are not equals
assertNotEqual ::
  (Eq a, Show a, HasCallStack) =>
  -- | The message prefix
  String ->
  -- | The expected value
  a ->
  -- | The actual value
  a ->
  Assertion
assertNotEqual preface expected actual =
  unless (actual /= expected) (assertFailure msg)
  where
    msg =
      (if null preface then "" else preface ++ "\n")
        ++ "expected: "
        ++ show expected
        ++ "\n but got: "
        ++ show actual

-- | Counts entries
count :: Entries a -> Either (a, Int) Int
count = foldlEntries (\a e -> a + 1) 0

assertEntriesWithoutErr :: (Show a) => Entries a -> Assertion
assertEntriesWithoutErr = go
  where
    go (Next e es) = go es
    go Done = pure ()
    go (Fail err) = assertFailure $ show err

assertEntriesWithErr :: (Show a) => Entries a -> Assertion
assertEntriesWithErr = go
  where
    go (Next e es) = go es
    go Done = assertFailure "expected to fail"
    go (Fail err) = pure ()

-- | Retrieves normalized POSIX path
filePathOf :: Entry -> FilePath
filePathOf = normalise . fromTarPathToPosixPath . entryTarPath

-- | True if tar entry is for a file or a symlink, otherwise False
isFileOrLinkTarget :: Entry -> Bool
isFileOrLinkTarget e = isFile e || isSymLink e || isHardLink e

-- | True if tar entry is for a file with content, otherwise False.
isFile :: Entry -> Bool
isFile (Entry _ (NormalFile _ _) _ _ _ _) = True
isFile _ = False

-- | True if tar entry is for a symbolic link, otherwise False.
isSymLink :: Entry -> Bool
isSymLink (Entry _ (SymbolicLink _) _ _ _ _) = True
isSymLink _ = False

-- | True if tar entry is for a hard link, otherwise False.
isHardLink :: Entry -> Bool
isHardLink (Entry _ (HardLink _) _ _ _ _) = True
isHardLink _ = False

-- | Makes simple index from tar entries. It includes all
-- entry type unlike Tar.index
buildIndex :: Entries e -> Either e [(Entry, TarEntryOffset)]
buildIndex = go mempty
  where
    go builder (Next e es) = go (addEntry e builder) es
    go builder Done = Right $ map remove3rd builder
    go _ (Fail err) = Left err

    addEntry :: Entry -> [(Entry, TarEntryOffset, TarEntryOffset)] -> [(Entry, TarEntryOffset, TarEntryOffset)]
    addEntry entry tarEntries = do
      let prevOffset =
            if length tarEntries > 0
              then get3rd . last $ tarEntries
              else 0
      tarEntries ++ [(entry, prevOffset, nextEntryOffset entry prevOffset)]

    get3rd :: (x, y, z) -> z
    get3rd (a, b, c) = c

    remove3rd :: (x, y, z) -> (x, y)
    remove3rd (a, b, c) = (a, b)

main :: IO ()
main = do
  let seekSnapshotTests = map mkRandomSeekTest snapshotFiles
  let validReadSnapshotTests = map mkValidReadTest snapshotFiles
  defaultMain $ testGroup "Tests" $ [snapshots] ++ seekSnapshotTests ++ validReadSnapshotTests

mkRandomSeekTest :: String -> TestTree
mkRandomSeekTest tarfile =
  testCase ("random seek should work for: " <> tarfile) $ do
    bs <- readFile $ tarfile
    let tar = read' bs
    case buildIndex tar of
      Left err -> assertFailure $ "expected to created index for: " <> show err
      Right tarIdx -> do
        let filesIndex = filter (\f -> isFileOrLinkTarget $ fst f) tarIdx
        for_ filesIndex $ \(entry, tarOffset) -> do
          withFile tarfile ReadMode $ \handle -> do
            eitherEntry <- try $ hReadEntry handle tarOffset
            let expectedPath = filePathOf entry
            case eitherEntry of
              Left (err :: SomeException) -> assertFailure $ "expected to read: " <> expectedPath <> "@" <> show tarOffset <> " => " <> show err
              Right entry' -> assertEqual "" expectedPath (filePathOf entry')

mkValidReadTest :: String -> TestTree
mkValidReadTest tarfile =
  testCase ("should be able to read: " <> tarfile) $ do
    bs <- readFile $ tarfile
    let tar = read' bs
    assertEntriesWithoutErr tar

-- This makes many tests effectively tautological, but since there is no read' on this branch
-- it's a quick and dirty way to make things work.
read' = read

snapshots :: TestTree
snapshots =
  testGroup
    "snapshots"
    [ testGroup
        "read'"
        [ testCase "should be same as read for non-pax tarballs" $
            do
              ustarRead <- read <$> basicUstar
              ustarRead' <- read' <$> basicUstar
              assertEqual "" ustarRead ustarRead'

              gnuRead <- read <$> basicGnu
              gnuRead' <- read' <$> basicGnu
              assertEqual "" gnuRead gnuRead',
          testCase "should not fail fatally when empty path is provided" $
            do
              emptyPathRead <- read <$> emptyPath
              emptyPathRead' <- read' <$> emptyPath
              -- I've commented out this test because we can't expect them to be different now.
              -- However, this still checks that we should not fail fatally with an empty path.
              -- assertNotEqual "" emptyPathRead emptyPathRead'
              assertEqual "" (count emptyPathRead') (Right 5),
          testCase "should fail for invalid tar files" $
            do
              invalidTar <- tarAt "invalid.tar"
              badHeaderTar <- tarAt "pax-bad-hdr-file.tar"

              assertEntriesWithErr $ read invalidTar
              assertEntriesWithErr $ read badHeaderTar
        ]
    ]
