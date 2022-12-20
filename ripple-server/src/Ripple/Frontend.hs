{-# LANGUAGE TemplateHaskell #-}

module Ripple.Frontend where

import Data.ByteString (ByteString)
import Data.FileEmbed
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Ripple.Frontend.TH
import System.Environment
import System.Exit

{-# ANN module "NOCOVER" #-}

frontendHome :: FilePath
frontendHome = $(frontendPath "Main.html")
