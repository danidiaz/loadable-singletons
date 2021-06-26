{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Options.Applicative qualified as O
import System.Directory
import System.FilePath

data Command = Pack deriving Show

commandParserInfo :: O.ParserInfo Command
commandParserInfo = 
    let packParserInfo :: O.ParserInfo Command
        packParserInfo = 
            O.info
            (pure Pack) 
            (O.fullDesc <> O.progDesc "Pack resource files into an sqlite database.")
        toplevel :: O.Parser Command
        toplevel = O.subparser (mconcat [ 
                O.command "pack" packParserInfo 
            ])
     in O.info toplevel (O.fullDesc <> O.progDesc "Resource file manager.") 

main :: IO ()
main = pure ()

