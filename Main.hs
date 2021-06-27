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
import Data.Coerce
import Data.Foldable

main :: IO ()
main = do
  Pack {packageFolder,exePath} <- O.execParser parserInfo
  roots <- filter isRoot <$> listDirectory packageFolder
  traverse_ putStrLn roots
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

rootPrefixes :: [String]
rootPrefixes = ["src", "lib", "app"]

isRoot :: FilePath -> Bool
isRoot path = getAny $ foldMap (coerce (isPrefixOf @Char)) rootPrefixes $ path

type ModuleFolder = String

resourceForest :: [FilePath] -> IO (Forest ModuleFolder) 
resourceForest = foldMap rootResoureForest
  where
    rootResoureForest :: FilePath -> IO (Forest ModuleFolder)
    rootResoureForest path = undefined

