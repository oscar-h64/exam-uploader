--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( concurrently_ )
import Control.Concurrent.Chan ( newChan, readChan, writeChan )
import Control.Monad ( forever, void )

import Data.Text ( Text )
import Data.UUID ( UUID, fromString )

import Options.Applicative

import Servant.Client.Core ( parseBaseUrl )

import System.FilePath ( splitFileName, takeFileName )
import System.FSNotify

import Warwick.AEP

--------------------------------------------------------------------------------

data Options = MkOptions {
    optSSC :: Text,
    optAssessment :: UUID,
    optFile :: FilePath,
    optInstance :: AEPInstance
}

options :: Parser Options
options = MkOptions <$> strOption (  long "ssc"
                                  <> metavar "SSC"
                                  <> help "The value of your AEP SSC cookie"
                                  )
                    <*> option auto (  long "assessment"
                                    <> metavar "ASSESSMENT"
                                    <> help "The UUID of the assessment in AEP"
                                    )
                    <*> strOption (  long "file"
                                  <> metavar "FILEPATH"
                                  <> help "The file to watch and upload"
                                  )
                    <*> option (maybeReader instanceFromString)
                            (  long "instance"
                            <> metavar "live|sandbox|URL"
                            <> help "The AEP instance to use"
                            <> value Live
                            <> showDefault
                            )

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

    let (dir, file) = splitFileName optFile

    let pred (Modified path _ False) = takeFileName path == file
        pred _ = False

    chan <- newChan

    let processThread = forever $ do
            event <- readChan chan
            let path = eventPath event
            res <- withAEP optInstance optSSC
                 $ uploadFile optAssessment path True
            
            -- Print if there was an error making the request
            either print (const $ putStrLn "File Uploaded Successfully") res

            -- Wait at least 5 seconds between uploads
            -- Otherwise AEP can 500 because of database locks
            threadDelay 5000000

    let watchThread = withManager $ \mgr -> do
            watchDir mgr dir pred (writeChan chan)

            putStrLn "Watcher started. Press enter to stop"
            void getLine

    concurrently_ processThread watchThread

--------------------------------------------------------------------------------
