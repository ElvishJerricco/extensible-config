{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens
import Control.Monad.Reader
import Data.Extensible.Config
import Data.Monoid

data MyConfig = MyConfig
  { _a :: Int
  , _b :: Int
  , _c :: Int
  , _d :: Int
  } deriving (Show, Eq, Ord)

makeLenses ''MyConfig

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests = [testGroup "Unit tests" huTests]

huTests :: [TestTree]
huTests =
  [ testCase "conf" $ conf @?= MyConfig
    { _a = 3
    , _b = 3
    , _c = 3
    , _d = 2
    }
  ]

conf :: MyConfig
conf = configure id (conf3 <> conf2 <> conf1 <> conf0)
 where
  -- Bootstrap the fixed point. Notice the lazy matching on the
  -- argument, which allows this function to evaluate to WHNF without
  -- recursing back on itself.
  conf0 = EndoM $ \(~MyConfig {..}) -> return MyConfig {..}

  conf1 = mempty & overriding %~ \super -> super & a .~ 2
                                                 & b .~ super ^. a

  conf2 = EndoM $ \super -> do
    self <- ask
    return $ super & c .~ self ^. b
                   & d .~ super ^. a

  conf3 = mempty & overriding . a .~ 3
