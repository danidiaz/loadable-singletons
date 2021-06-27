{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Options.Applicative qualified as O
import System.Directory
import System.FilePath

data Command = Pack {baseFolder :: FilePath, exePath :: FilePath} deriving (Show)

parserInfo :: O.ParserInfo Command
parserInfo =
  let packParserInfo :: O.ParserInfo Command
      packParserInfo =
        O.info
          ( O.helper
              <*> ( Pack
                      <$> O.strArgument (O.metavar "PACKAGE_FOLDER" <> O.help "Base package folder on which to seek resources.")
                      <*> O.strArgument (O.metavar "EXECUTABLE_PATH" <> O.help "Path to the executable.")
                  )
          )
          (O.fullDesc <> O.progDesc "Pack resource files into an sqlite database next to the executable.")
      toplevel :: O.Parser Command
      toplevel =
        O.subparser
          ( mconcat
              [ O.command "pack" packParserInfo
              ]
          )
   in O.info (O.helper <*> toplevel) (O.fullDesc <> O.progDesc "A resource file packer.")

main :: IO ()
main = do
  _ <- O.execParser parserInfo
  pure ()
