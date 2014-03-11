module Util.File (
  recurseDirs
, allDirs
, allFiles
, mkdir_p
, mkdir
) where

import Control.Monad
import System.Directory
import System.FilePath ((</>))

recurseDirs :: FilePath -> IO [FilePath]
recurseDirs baseDir = do
    contents <- getDirectoryContents baseDir
    let contentChildren = filter (`notElem` [".", ".."]) contents
    paths <- forM contentChildren $ returnOrRecurse baseDir
    return $ concat paths
    where
        returnOrRecurse baseDir fileOrDir = do
            let path = baseDir </> fileOrDir
            isDir <- doesDirectoryExist path
            if isDir
                then recurseDirs path
                else return [path]

allDirs :: FilePath -> IO [FilePath]
allDirs dir = do
    rawDirs <- getDirectoryContents dir >>= filterM doesDirectoryExist
    let dirs = filter (`notElem` [".", ".."]) rawDirs -- remove ./ and ../
    return dirs

allFiles :: FilePath -> IO [FilePath]
allFiles dir = getDirectoryContents dir >>= filterM doesFileExist

mkdir_p :: [FilePath] -> IO FilePath
mkdir_p = foldM (\acc i -> mkdir (acc </> i) >> return (acc </> i)) ""

mkdir :: FilePath -> IO ()
mkdir fp = do
  b <- doesDirectoryExist fp
  if b then
    return ()
  else
    createDirectory fp
