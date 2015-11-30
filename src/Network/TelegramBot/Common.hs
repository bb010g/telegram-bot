{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.TelegramBot.Common where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char
import Data.Constraint
import Data.Monoid
import Data.Void

tgJSONOptions :: Options
tgJSONOptions = defaultOptions
    { constructorTagModifier = toSnakeCase
    , omitNothingFields = True
    }

toSnakeCase :: String -> String
toSnakeCase = go True
    where go _     ""     = ""
          go True  (c:cs) = toLower c : go False cs
          go False (c:cs) = if isUpper c then '_' : toLower c : go False cs else c : go False cs

snakeDrop :: Int -> String -> String
snakeDrop n s = toSnakeCase (drop n s)

data a :+: b = L a | R b
    deriving (Eq, Ord, Show, Read)

type family OneOf (as :: [*]) where
    -- OneOf '[] = Void
    OneOf '[x] = x
    OneOf (x ': xs) = x :+: OneOf xs

instance (FromJSON a, FromJSON b) => FromJSON (a :+: b) where
    parseJSON v = L <$> (parseJSON v :: Parser a) <|> R <$> (parseJSON v :: Parser b)

instance (ToJSON a, ToJSON b) => ToJSON (a :+: b) where
    toJSON (L x) = toJSON x
    toJSON (R y) = toJSON y

makePrisms ''(:+:)
makeLenses ''(:+:)
