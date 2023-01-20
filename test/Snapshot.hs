module Main where

import Codec.Archive.Tar.Read (read, read')
import Codec.Archive.Tar.Types (foldlEntries, Entries)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString, readFile)
import Prelude hiding (read, readFile)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, HasCallStack, testCase)

basicUstar :: IO ByteString
basicUstar =
    readFile "test/testdata/basic_ustar.tar"

basicPax :: IO ByteString
basicPax =
    readFile "test/testdata/basic_pax.tar"

basicGnu :: IO ByteString
basicGnu =
    readFile "test/testdata/basic_gnu.tar"

emptyPath :: IO ByteString
emptyPath =
    readFile "test/testdata/emptypath.tar"

main :: IO ()
main = defaultMain $ testGroup "Tests" [snapshots]

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

count :: Entries a -> Either (a, Int) Int
count = foldlEntries (\a e -> a + 1) 0

snapshots = testGroup "snapshots" [
        testGroup "read'" [
              testCase "should ignore PAX header entries" $
                do
                    paxRead <- read <$> basicPax
                    paxRead' <- read' <$> basicPax
                    assertNotEqual "" paxRead paxRead'
                    assertEqual "" (count paxRead) (Right 20)
                    assertEqual "" (count paxRead') (Right 10)

            , testCase "should be same as read for non-pax tarballs" $
                do
                    ustarRead <- read <$> basicUstar
                    ustarRead' <- read <$> basicUstar
                    assertEqual "" ustarRead ustarRead'

                    gnuRead <- read <$> basicGnu
                    gnuRead' <- read <$> basicGnu
                    assertEqual "" gnuRead gnuRead'

            , testCase "should not fail fatally when empty path is provided" $
                do
                    emptyPathRead <- read <$> emptyPath
                    emptyPathRead' <- read' <$> emptyPath
                    assertNotEqual "" emptyPathRead emptyPathRead'
                    assertEqual "" (count emptyPathRead') (Right 5)
        ]
    ]