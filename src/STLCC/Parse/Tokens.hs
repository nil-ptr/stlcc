{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
module STLCC.Parse.Tokens where

import           Data.Proxy
import           Text.Megaparsec


----------------------------------------------------------------------
---                          Basic Tokens                          ---
----------------------------------------------------------------------

lambdaTok :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
lambdaTok = satisfy (\x -> x == '\\' || x == 'λ') <?> "lambda"

arrowTok :: (MonadParsec e s m
            , Token s ~ Char) => m (Tokens s)
arrowTok = asciiArrow <|> unicodeArrow
  where asciiArrow :: (MonadParsec e s m
                      , Token s ~ Char) => m (Tokens s)
        asciiArrow = do
          single '-'
          single '>'
          pure (tokensToChunk Proxy "->")

        unicodeArrow  :: (MonadParsec e s m, Token s ~ Char) => m (Tokens s)
        unicodeArrow = tokensToChunk Proxy <$> single ['→']
