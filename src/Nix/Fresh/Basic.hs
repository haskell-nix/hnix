{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Fresh.Basic where

import           Control.Monad.Reader
import           Nix.Effects
import           Nix.Render
import           Nix.Fresh
import           Nix.Fresh.Stable
import           Nix.Value

type StdIdT = FreshStableIdT
