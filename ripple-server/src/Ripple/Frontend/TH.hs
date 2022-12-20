{-# LANGUAGE QuasiQuotes #-}

module Ripple.Frontend.TH where

import Data.ByteString (ByteString)
import Data.FileEmbed
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Environment
import System.Exit

{-# ANN module "NOCOVER" #-}

frontendDir :: Q FilePath
frontendDir = do
  md <- runIO $ lookupEnv "RIPPLE_FRONTEND"
  case md of
    Nothing -> runIO $ die "WARNING: Building without dependency graph, set NIXOS_MODULE_DOCS to build them during development."
    Just fp -> do
      runIO $ putStrLn $ "Building with frontend graph at " <> fp
      pure fp

frontendPath :: FilePath -> Q Exp
frontendPath rp = do
  dp <- frontendDir
  let p = dp <> "/" <> rp
  [|p|]
