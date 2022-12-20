{-# LANGUAGE TemplateHaskell #-}

module Ripple.Frontend where

import Ripple.Frontend.TH

{-# ANN module "NOCOVER" #-}

frontendHome :: FilePath
frontendHome = $(frontendPath "index.html")

frontendApp :: FilePath
frontendApp = $(frontendPath "Main.min.js")

frontendStyle :: FilePath
frontendStyle = $(frontendPath "css/styles.css")

frontendWebmanifest :: FilePath
frontendWebmanifest = $(frontendPath "site.webmanifest")
