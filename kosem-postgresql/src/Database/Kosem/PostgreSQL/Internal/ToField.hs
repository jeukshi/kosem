{-# LANGUAGE DefaultSignatures #-}

module Database.Kosem.PostgreSQL.Internal.ToField where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Word (Word32)
import Language.Haskell.TH
import PostgreSQL.Binary.Encoding

-- TODO is ToField even a good name?
-- maybe Serialize? ToParam? Param?
-- what about FromField then...
class ToField a where
    toField :: a -> ByteString
    {-# MINIMAL toField #-}

    toField'Internal :: a -> Maybe ByteString
    default toField'Internal :: a -> Maybe ByteString
    toField'Internal = Just . toField

instance ToField Text where
    toField :: Text -> ByteString
    toField = encodingBytes . text_strict

instance ToField Bool where
    toField :: Bool -> ByteString
    toField = encodingBytes . bool

instance ToField Int where
    toField :: Int -> ByteString
    toField = encodingBytes . int8_int64 . fromIntegral

instance (ToField a) => ToField (Maybe a) where
    toField :: (ToField a) => Maybe a -> ByteString
    toField = error "unexpected null"

    toField'Internal :: (ToField a) => Maybe a -> Maybe ByteString
    toField'Internal = fmap toField
