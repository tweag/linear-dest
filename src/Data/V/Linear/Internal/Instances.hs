{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-- | This module contains all instances for V
module Data.V.Linear.Internal.Instances where

import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Data.Functor.Linear.Internal.Functor as Data
import Data.V.Linear.Internal (V (..))
import qualified Data.V.Linear.Internal as V
import GHC.TypeLits

-- # Instances of V
-------------------------------------------------------------------------------

instance Data.Functor (V n) where
  fmap = V.map

instance KnownNat n => Data.Applicative (V n) where
  pure = V.pure
  a <*> b = a V.<*> b

