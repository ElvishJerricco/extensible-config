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

configureM :: MonadFix m => (super -> m self) -> EndoM (ReaderT self m) super -> m self
configureM f e = mfix (runReaderT (lift . f =<< fixEndoM e))

overriding :: Setter' (EndoM m s) s
overriding = sets $ \f (EndoM g) -> EndoM (g . f)

--------------------------------------------------------------------------------

type Endo = EndoM Identity

endo :: (a -> a) -> Endo a
endo f = EndoM (Identity . f)

appEndo :: Endo a -> a -> a
appEndo (EndoM f) = runIdentity . f

fixEndo :: Endo a -> a
fixEndo = runIdentity . fixEndoM

type Configurable self a = EndoM (Reader self) a

configure :: (a -> self) -> Configurable self a -> self
configure f = runIdentity . configureM (Identity . f)
