{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Options.Applicative qualified as O
import System.Directory
import System.FilePath

data Command = Pack {baseFolder :: FilePath, exePath :: FilePath} deriving (Show)

parserInfo :: O.ParserInfo Command
parserInfo =
  let pack :: O.ParserInfo Command
      pack =
        makeParserInfo
          ( Pack
              <$> O.strArgument (O.metavar "PACKAGE_FOLDER" <> O.help "Base package folder on which to seek resources.")
              <*> O.strArgument (O.metavar "EXECUTABLE_PATH" <> O.help "Path to the executable.")
          )
          (O.progDesc "Pack resource files into an sqlite database next to the executable.")
      toplevel :: O.ParserInfo Command
      toplevel =
        makeParserInfo
          ( O.subparser
              ( mconcat
                  [ O.command "pack" pack
                  ]
              )
          )
          (O.progDesc "A resource file packer.")
      makeParserInfo p mods = O.info (O.helper <*> p) (O.fullDesc <> mods)
   in toplevel

main :: IO ()
main = do
  _ <- O.execParser parserInfo
  pure ()
