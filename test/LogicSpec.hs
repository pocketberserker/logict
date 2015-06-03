module LogicSpec where

import Control.Monad
import Control.Monad.Writer.Strict
import Control.Monad.Logic
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "MonadLogic" $ do
    prop "split empty" $ \_ ->
      msplit mzero == return Nothing
    prop "split value" $ \a m ->
      msplit (return a `mplus` m) == return (Just (a, m))
    prop "reclect" $ \m ->
      msplit m >>= reflect

