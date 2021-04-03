--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Control.Monad ( void )

import Data.Maybe
import Data.Text ( Text )
import Data.UUID ( UUID, fromString )

import Options.Applicative

import Servant.Client.Core ( parseBaseUrl )

import System.Exit
import System.FilePath ( splitFileName, takeFileName )
import System.FSNotify

import Warwick.AEP

--------------------------------------------------------------------------------

data Options = MkOptions {
    optSSC :: Text,
    optMAssessment :: Maybe UUID,
    optFile :: FilePath,
    optMInstance :: Maybe AEPInstance
}

options :: Parser Options
options = MkOptions <$> strOption (  long "ssc"
                                  <> metavar "SSC"
                                  <> help "The value of your AEP SSC cookie"
                                  )
                    <*> fmap fromString (strOption
                            (  long "assessment"
                            <> metavar "ASSESSMENT"
                            <> help "The UUID of the assessment in AEP"
                            ))
                    <*> strOption (  long "file"
                                  <> metavar "FILEPATH"
                                  <> help "The file to watch and upload"
                                  )
                    <*> fmap instanceFromString (strOption
                            (  long "instance"
                            <> metavar "live|sandbox|URL"
                            <> help "The AEP instance to use"
                            <> value "live"
                            <> showDefault
                            ))

optInfo :: ParserInfo Options
optInfo = info (options <**> helper) 
    (  fullDesc
    <> header "AEP Exam Uploader: Continously upload an answer file to the AEP"
    )

--------------------------------------------------------------------------------

instanceFromString :: String -> Maybe AEPInstance
instanceFromString "live" = pure Live
instanceFromString "sandbox" = pure Sandbox
instanceFromString url = CustomInstance <$> parseBaseUrl url

--------------------------------------------------------------------------------

main :: IO ()
main = do
    MkOptions{..} <- execParser optInfo

    optAssessment <- case optMAssessment of
        Nothing -> putStrLn "Invalid UUID for assessment" >> exitFailure
        Just uuid -> pure uuid

    optInstance <- case optMInstance of
        Nothing -> putStrLn "Invalid AEP instance" >> exitFailure
        Just inst -> pure inst

    let (dir, file) = splitFileName optFile

    let pred (Modified path _ False) = takeFileName path == file
        pred _ = False

    let action event = do
            let path = eventPath event
            res <- withAEP optInstance optSSC
                 $ uploadFile optAssessment path True
            
            -- Print if there was an error making the request
            either print (const $ putStrLn "File Uploaded Successfully") res

    withManager $ \mgr -> do
        watchDir mgr dir pred action

        putStrLn "Watcher started. Press enter to stop"
        void $ getLine

--------------------------------------------------------------------------------
