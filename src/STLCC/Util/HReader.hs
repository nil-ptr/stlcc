{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module STLCC.Util.HReader where

import           Control.Monad.Reader
import           Text.Megaparsec
import           Text.Megaparsec.Internal

-- | A variation on the class with the same name in Richard
-- Eisenberg's functional pearl \"Stitch\".
--
-- Note that I've opted to track both environment types in the class
-- head, instead of hiding one behind an associated type family. This
-- was necessary in order to please the type checker.
class (Monad m1, Monad m2)
      => MonadHReader r1 m1 r2 m2 | m1 -> r1, m2 -> r2 where
  hlocal :: (r1 -> r2) -> m2 a -> m1 a

-- | The definition of 'hlocal' here is identical to the definition of
-- 'local' in the corresponding 'MonadReader' instance.
instance Monad m => MonadHReader r1 (ReaderT r1 m) r2 (ReaderT r2 m) where
  hlocal f (ReaderT g) = ReaderT (g . f)

-- | The definition of 'hlocal' here is identical to the definition of
-- 'local' in the corresponding 'MonadReader' instance. Relies on
-- definitions from "Text.Megaparsec.Internal", unfortunately.
instance (MonadHReader r1 m1 r2 m2, Stream s)
      =>  MonadHReader r1 (ParsecT e s m1) r2 (ParsecT e s m2) where
  hlocal f p = mkPT $ \s -> hlocal f (runParsecT p s)

-- -- | MonadHReader, taken from Eisenberg's functional pearl.
-- class Monad m => MonadHReader r1 m | m -> r1 where
--   type SetEnv r2 m :: * -> *
--   hlocal :: Monad (SetEnv r2 m)
--          => (r1 -> r2)
--          -> SetEnv r2 m a
--          -> m a

-- instance Monad m => MonadHReader r1 (ReaderT r1 m) where
--   type SetEnv r2 (ReaderT r1 m) = ReaderT r2 m
--   hlocal f (ReaderT g) = ReaderT (g . f)

-- instance (MonadHReader r1 m, Stream s)
--          => MonadHReader r1 (ParsecT e s m) where
--   type SetEnv r2 (ParsecT e s m) = ParsecT e s (SetEnv r2 m)
--   hlocal f m = mkPT $ \s -> hlocal f (runParsecT m s)



-- | Copied from "Text.Megaparsec.Internal"
mkPT :: Monad m => (State s e -> m (Reply e s a)) -> ParsecT e s m a
mkPT k = ParsecT $ \s cok cerr eok eerr -> do
  (Reply s' consumption result) <- k s
  case consumption of
    Consumed ->
      case result of
        OK    x -> cok x s' mempty
        Error e -> cerr e s'
    Virgin ->
      case result of
        OK    x -> eok x s' mempty
        Error e -> eerr e s'
