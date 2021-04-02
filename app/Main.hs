--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Control.Monad ( void )

import Data.Maybe
import Data.Text ( Text )
import Data.UUID ( UUID, fromString )

import Options.Applicative

import System.FilePath ( splitFileName, takeFileName )
import System.FSNotify

import Lib

--------------------------------------------------------------------------------

data Options = MkOptions {
    optSSC :: Text,
    optAssessment :: UUID,
    optFile :: FilePath
}

options :: Parser Options
options = MkOptions <$> strOption (  long "ssc"
                                  <> metavar "SSC"
                                  <> help "The value of your AEP SSC cookie"
                                  )
                    <*> fmap (fromJust . fromString) (strOption $
                            (  long "assessment"
                            <> metavar "ASSESSMENT"
                            <> help "The UUID of the assessment in AEP"
                            ))
                    <*> strOption (  long "file"
                                  <> metavar "FILEPATH"
                                  <> help "The file to watch and upload"
                                  )

optInfo :: ParserInfo Options
optInfo = info (options <**> helper) 
    (  fullDesc
    <> header "AEP Exam Uploader: Continously upload an answer file to the AEP"
    )

--------------------------------------------------------------------------------

main :: IO ()
main = do
    MkOptions{..} <- execParser optInfo

    let (dir, file) = splitFileName optFile

    let pred (Modified path _ False) = takeFileName path == file
        pred _ = False

    withManager $ \mgr -> do
        watchDir mgr dir pred print

        putStrLn "Watcher started. Press enter to stop"
        void $ getLine

--------------------------------------------------------------------------------
