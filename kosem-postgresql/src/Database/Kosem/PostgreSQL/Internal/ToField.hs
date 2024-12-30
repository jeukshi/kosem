{-# LANGUAGE DefaultSignatures #-}

module Database.Kosem.PostgreSQL.Internal.ToField where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Int (Int8)
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

    toFieldWithLen :: Int8 -> a -> ByteString
    default toFieldWithLen :: Int8 -> a -> ByteString
    toFieldWithLen _ = toField

    toField'Internal :: a -> Maybe ByteString
    default toField'Internal :: a -> Maybe ByteString
    toField'Internal = Just . toField

    toFieldWithLen'Internal :: Int8 -> a -> Maybe ByteString
    default toFieldWithLen'Internal :: Int8 -> a -> Maybe ByteString
    toFieldWithLen'Internal n = Just . toFieldWithLen n

instance ToField Text where
    toField :: Text -> ByteString
    toField = encodingBytes . text_strict

instance ToField Bool where
    toField :: Bool -> ByteString
    toField = encodingBytes . bool

instance ToField Int where
    toField :: Int -> ByteString
    toField = error "TODO errors later"
    toFieldWithLen :: Int8 -> Int -> ByteString
    toFieldWithLen 8 = encodingBytes . int8_int64 . fromIntegral
    toFieldWithLen 4 = encodingBytes . int4_int32 . fromIntegral
    toFieldWithLen 2 = encodingBytes . int2_int16 . fromIntegral
    toFieldWithLen n = error $ "TODO: not implemented for length: " <> show n

instance (ToField a) => ToField (Maybe a) where
    toField :: (ToField a) => Maybe a -> ByteString
    toField = error "unexpected null"

    toField'Internal :: (ToField a) => Maybe a -> Maybe ByteString
    toField'Internal = fmap toField

    toFieldWithLen'Internal :: (ToField a) => Int8 -> Maybe a -> Maybe ByteString
    toFieldWithLen'Internal n = fmap (toFieldWithLen n)
