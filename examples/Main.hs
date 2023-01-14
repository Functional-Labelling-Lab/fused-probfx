{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}

module Main where

import           LDA                (mhLDA, simLDA)
import           LinRegr            (inferLwLinRegr, inferMhLinRegr,
                                     simulateLinRegr)
import           LogRegr            (inferLwLogRegr, inferMHLogRegr,
                                     simulateLogRegr)
import           Radon              (mhRadon, simRadon)
import           Sampler            (sampleIOFixed)
import           School             (mhSchool)
import           SIR                (inferSIR, simulateSIR, simulateSIRS,
                                     simulateSIRSV)
import           System.Environment (getArgs)

printThenWrite :: Show a => String -> a -> IO ()
printThenWrite f a = print a >> writeFile f (show a)

availableCommands = "[simLinRegr, lwLinRegr, mhLinRegr, simSIR, simSIRS, simSIRSV, mhSIR, simLogRegr, lwLogRegr, mhLogRegr, simLDA, mhLDA, simRadon, mhRadon, mhSchool]"

parseArgs :: String -> String -> IO ()
parseArgs f cmd = case cmd of
  "simLinRegr"  -> sampleIOFixed simulateLinRegr >>= printThenWrite f
  "lwLinRegr"   -> sampleIOFixed inferLwLinRegr >>= printThenWrite f
  "simSIR"      -> sampleIOFixed simulateSIR >>= printThenWrite f
  "simSIRS"     -> sampleIOFixed simulateSIRS >>= printThenWrite f
  "simSIRSV"    -> sampleIOFixed simulateSIRSV >>= printThenWrite f
  "mhSIR"       -> sampleIOFixed inferSIR >>= printThenWrite f

  "mhLinRegr"   -> sampleIOFixed inferMhLinRegr >>= printThenWrite f
  "simLogRegr"  -> sampleIOFixed simulateLogRegr >>= printThenWrite f
  "lwLogRegr"   -> sampleIOFixed inferLwLogRegr >>= printThenWrite f
  "mhLogRegr"   -> sampleIOFixed inferMHLogRegr >>= printThenWrite f
  "simLDA"      -> sampleIOFixed simLDA >>= printThenWrite f
  "mhLDA"       -> sampleIOFixed mhLDA >>= printThenWrite f
  "simRadon"    -> sampleIOFixed simRadon >>= printThenWrite f
  "mhRadon"     -> sampleIOFixed mhRadon >>= printThenWrite f
  "mhSchool"    -> sampleIOFixed mhSchool >>= printThenWrite f
  _             -> putStrLn $ "unrecognised command: " ++ cmd ++ "\n"
                           ++ "available commands: " ++ availableCommands

main :: IO ()
main = do
  args <- getArgs
  case args of []       -> print $ "no arguments provided to ProbFX. Available arguments: " ++ availableCommands
               (f:a:as) -> parseArgs f a
               _        -> print $ "no arguments provided to ProbFX. Available arguments: " ++ availableCommands
