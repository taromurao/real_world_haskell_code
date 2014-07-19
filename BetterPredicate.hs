{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module BetterPredicate (betterFind) where

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where
        check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

type Predicate = FilePath
    -> Permissions
    -> Maybe Integer
    -> UTCTime
    -> Bool

type InfoP a = FilePath
    -> Permissions
    -> Maybe Integer
    -> UTCTime
    -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _     = -1

-- Lifting functions
--
liftP :: (a -> b-> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

andP = liftP2 (&&)
orP = liftP2 (||)

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

-- Operators
--
(==?) = equalP
(&&?) = andP
(>?) = greaterP
(<?) = lesserP
infix 4 ==?
infix 3 &&?
infix 4 >?
infix 4 <?

-- private
--
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(_ :: IOError) -> return Nothing) $
    bracket (openFile path ReadMode) hClose $ \h -> do
        size <- hFileSize h
        return (Just size)

