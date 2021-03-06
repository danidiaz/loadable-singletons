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
  resources <- foldMap resourceForest0 roots
  putStrLn $ drawForest (fmap (fmap show) resources)
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

type Section = String

data Catalog = Catalog {
        txt :: [(Section, FilePath)]
    ,   sql :: [(Section, FilePath)]
    ,   json :: [(Section, FilePath)]
    } deriving Show

instance Semigroup Catalog where
    Catalog a b c <> Catalog a' b' c' = Catalog (a <> a') (b <> b') (c <> c')

instance Monoid Catalog where
    mempty = Catalog [] [] []

parseCatalog :: FilePath -> Catalog
parseCatalog fullPath =
    let (dir,filename) = splitFileName fullPath 
        (bare,ext) = splitExtension filename
        r = [(bare,fullPath)]
     in case ext of
        ".txt" -> mempty {txt = r} 
        ".sql" -> mempty {sql = r}
        ".json" -> mempty {json = r}
        _ -> mempty

resourceForest0 :: FilePath -> IO (Forest (FilePath,Catalog)) 
resourceForest0 path = do
    (_, dirs) <- filesAndDirs path
    traverse resourceTree dirs

resourceTree :: FilePath -> IO (Tree (FilePath,Catalog)) 
resourceTree path = do
    (files, dirs) <- filesAndDirs path
    let catalog = foldMap parseCatalog files
    levelBelow <- traverse resourceTree dirs 
    pure $ Node (takeFileName path,catalog) levelBelow

filesAndDirs :: FilePath -> IO ([FilePath],[FilePath])
filesAndDirs base = do
    childPaths <- do
        entries <- listDirectory base
        pure $ do entry <- entries
                  pure $ base </> entry
    files <- filterM doesFileExist childPaths
    dirs <- filterM doesDirectoryExist childPaths
    pure (files, dirs)

