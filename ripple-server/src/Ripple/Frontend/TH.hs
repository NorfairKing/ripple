{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Ripple.Frontend.TH where

import Language.Haskell.TH
import System.Environment
import System.Exit

{-# ANN module "NOCOVER" #-}

frontendDir :: Q FilePath
frontendDir = do
  md <- runIO $ lookupEnv "RIPPLE_FRONTEND"
  case md of
    Nothing -> runIO $ die "Cannot build the backend without setting RIPPLE_FRONTEND"
    Just fp -> do
      runIO $ putStrLn $ "Building with frontend at " <> fp
      pure fp

frontendPath :: FilePath -> Q Exp
frontendPath rp = do
  dp <- frontendDir
  let p = dp <> "/" <> rp
  [|p|]
