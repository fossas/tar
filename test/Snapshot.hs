{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Codec.Archive.Tar.Index (build, toList, hReadEntry, TarEntryOffset, nextEntryOffset)
import Codec.Archive.Tar.Read (read, read')
import Codec.Archive.Tar.Types (foldlEntries, Entries(..), EntryContent(..))
import Codec.Archive.Tar.Entry (Entry (..), TarPath, entryPath, fromTarPathToPosixPath)
import Data.Foldable (asum, for_)
import Data.Traversable (forM)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString, readFile)
import Prelude hiding (read, readFile)

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, HasCallStack, testCase)
import Control.Exception (try, SomeException)
import System.IO (Handle, IOMode (ReadMode), withFile)
import System.FilePath.Posix (normalise)

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
snapshotFiles = map testCaseFileAt [
        "basic_pax.tar",
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
assertNotEqual
  :: (Eq a, Show a, HasCallStack)
  => String -- ^ The message prefix
  -> a      -- ^ The expected value
  -> a      -- ^ The actual value
  -> Assertion
assertNotEqual preface expected actual =
  unless (actual /= expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n but got: " ++ show actual

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
            let prevOffset = if length tarEntries > 0 
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
                let filesIndex = filter (\f -> isFileOrLinkTarget $ fst f)  tarIdx
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

snapshots :: TestTree
snapshots = testGroup "snapshots" [
        testGroup "read'" [
              testCase "should be same as read for non-pax tarballs" $
                do
                    ustarRead <- read <$> basicUstar
                    ustarRead' <- read' <$> basicUstar
                    assertEqual "" ustarRead ustarRead'

                    gnuRead <- read <$> basicGnu
                    gnuRead' <- read' <$> basicGnu
                    assertEqual "" gnuRead gnuRead'

            , testCase "should not fail fatally when empty path is provided" $
                do
                    emptyPathRead <- read <$> emptyPath
                    emptyPathRead' <- read' <$> emptyPath
                    assertNotEqual "" emptyPathRead emptyPathRead'
                    assertEqual "" (count emptyPathRead') (Right 5)

            , testCase "should fail for invalid tar files" $
                do
                    invalidTar <- tarAt "invalid.tar"
                    badHeaderTar <- tarAt "pax-bad-hdr-file.tar"
                    
                    assertEntriesWithErr $ read invalidTar
                    assertEntriesWithErr $ read badHeaderTar
        ]
    ]