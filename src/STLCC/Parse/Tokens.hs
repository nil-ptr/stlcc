{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  STLCCTest.Tests.Parse
-- Copyright   :  Nils Gustafsson 2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines some basic tokens / lexemes for use in "STLCC.Parse".
module STLCC.Parse.Tokens
  (
    lambdaTok
  , arrowTok
  ) where

import           Data.Proxy
import           Text.Megaparsec


----------------------------------------------------------------------
---                          Basic Tokens                          ---
----------------------------------------------------------------------

-- | Matches either @\\\\@ or @λ@.
lambdaTok :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
lambdaTok = satisfy (\x -> x == '\\' || x == 'λ') <?> "lambda"

-- | Matches either @->@ or @→@.
arrowTok :: (MonadParsec e s m
            , Token s ~ Char) => m (Tokens s)
arrowTok = (asciiArrow <|> unicodeArrow) <?> "arrow"
  where asciiArrow :: (MonadParsec e s m
                      , Token s ~ Char) => m (Tokens s)
        asciiArrow = do
          single '-'
          single '>'
          pure (tokensToChunk Proxy "->")

        unicodeArrow  :: (MonadParsec e s m, Token s ~ Char) => m (Tokens s)
        unicodeArrow = tokensToChunk Proxy <$> single ['→']
