--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
import Control.Concurrent ( threadDelay, readMVar )
import Control.Concurrent.Async ( concurrently_ )
import Control.Concurrent.MVar ( newEmptyMVar, takeMVar, tryPutMVar )
import Control.Monad ( forever, void )

import Data.Text ( Text )
import Data.UUID ( UUID, fromString )

import Options.Applicative

import Servant.Client.Core ( parseBaseUrl )

import System.FilePath ( splitFileName, takeFileName )
import System.FSNotify ( Event(..), eventPath, watchDir, withManager )

import Warwick.AEP ( AEPInstance(..), uploadFile, withAEP )

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

    mVar <- newEmptyMVar

    let processThread = forever $ do
            event <- readMVar mVar

            -- Wait at least 5 seconds between uploads, otherwise AEP can 500
            -- because of database locks. This goes after a change is read, so
            -- that the file is finished saving, and multiple events triggered
            -- by the same save are ignored
            threadDelay 5000000

            -- Empty MVar after 5 seconds
            void $ takeMVar mVar

            let path = eventPath event
            res <- withAEP optInstance optSSC
                 $ uploadFile optAssessment path True
            
            -- Print if there was an error making the request
            either print (const $ putStrLn "File Uploaded Successfully") res

    let watchThread = withManager $ \mgr -> do
            watchDir mgr dir pred (void . tryPutMVar mVar)

            putStrLn "Watcher started. Press enter to stop"
            void getLine

    concurrently_ processThread watchThread

--------------------------------------------------------------------------------
