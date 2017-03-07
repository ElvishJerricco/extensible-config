{-# LANGUAGE RankNTypes #-}

module Data.Extensible.Config where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Semigroup hiding (Endo(..))

newtype EndoM m a = EndoM { appEndoM :: a -> m a }

instance Monad m => Semigroup (EndoM m a) where
  EndoM f <> EndoM g = EndoM (f <=< g)

-- | Super-most is right-most
instance Monad m => Monoid (EndoM m a) where
  mempty = EndoM return
  EndoM f `mappend` EndoM g = EndoM (f <=< g)

fixEndoM :: MonadFix m => EndoM m a -> m a
fixEndoM (EndoM f) = mfix f

configureM :: MonadFix m => (a -> self) -> EndoM (ReaderT self m) a -> m self
configureM f e = mfix (runReaderT (f <$> fixEndoM e))

overriding :: Setter' (EndoM m s) s
overriding = sets $ \f (EndoM g) -> EndoM (g . f)

--------------------------------------------------------------------------------

type Endo = EndoM Identity

appEndo :: Endo a -> a -> a
appEndo (EndoM f) = runIdentity . f

fixEndo :: Endo a -> a
fixEndo = runIdentity . fixEndoM

type Configurable self a = EndoM (Reader self) a

configure :: (a -> self) -> Configurable self a -> self
configure f = runIdentity . configureM f