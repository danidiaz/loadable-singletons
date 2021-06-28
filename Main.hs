{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Options.Applicative qualified as O
import System.Directory
import System.FilePath
import Data.Tree
import Data.List
import Data.Monoid
import Control.Monad
import Data.Coerce
import Data.Foldable
import Data.Functor.Compose

main :: IO ()
main = do
  Pack {packageFolder,exePath} <- O.execParser parserInfo
  roots <- do 
    candidates <- listDirectory packageFolder
    filterM isRoot $ map (packageFolder </>) candidates
  traverse_ putStrLn roots
  resources <- foldMap resourceForest roots
  putStrLn $ drawForest resources
  pure ()

data Command = Pack {packageFolder :: FilePath, exePath :: FilePath} deriving (Show)

parserInfo :: O.ParserInfo Command
parserInfo =
  let toplevel :: O.ParserInfo Command
      toplevel =
        makeParserInfo
          ( O.subparser
              ( mconcat
                  [ O.command "pack" pack
                  ]
              )
          )
          (O.progDesc "A resource file packer.")
      pack :: O.ParserInfo Command
      pack =
        makeParserInfo
          ( Pack
              <$> O.strArgument (O.metavar "PACKAGE_FOLDER" <> O.help "Base package folder on which to seek resources.")
              <*> O.strArgument (O.metavar "EXECUTABLE_PATH" <> O.help "Path to the executable.")
          )
          (O.progDesc "Pack resource files into an sqlite database next to the executable.")
      makeParserInfo p mods = O.info (O.helper <*> p) (O.fullDesc <> mods)
   in toplevel

isRoot :: FilePath -> IO Bool
isRoot path = do
    isFolder <- doesDirectoryExist path
    let Any prefixed = foldMap (coerce (isPrefixOf @Char)) rootPrefixes (takeFileName path)
    pure $ isFolder && prefixed

rootPrefixes :: [String]
rootPrefixes = ["src", "lib", "app"]

type ModuleFolder = String

resourceForest :: FilePath -> IO (Forest FilePath) 
resourceForest path = do
        (directories, files) <- do 
            candidates <- listDirectory path
            let candidatesFullPath = (path </>) <$> candidates
            directories <- filterM doesDirectoryExist candidatesFullPath 
            files <- filterM doesFileExist candidatesFullPath 
            pure (directories, files)
        let buildNode d = do
                under <- resourceForest d
                pure $ Node d under
        traverse buildNode directories 

