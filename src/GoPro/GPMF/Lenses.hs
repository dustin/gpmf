{-# LANGUAGE TemplateHaskell #-}

module GoPro.GPMF.Lenses where

import           Control.Lens
import           GoPro.GPMF

makePrisms ''Value
