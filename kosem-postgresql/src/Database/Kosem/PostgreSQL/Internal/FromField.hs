{-# LANGUAGE DefaultSignatures #-}

module Database.Kosem.PostgreSQL.Internal.FromField where

import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Exts (Any)
import PostgreSQL.Binary.Decoding
import Unsafe.Coerce (unsafeCoerce)

class FromField a where
    -- TODO We shoud prolly return Either
    -- and allow users to signal failure that way. TBD
    parseField :: ByteString -> a
    {-# MINIMAL parseField #-}

    parseField'Internal :: Maybe ByteString -> a
    default parseField'Internal :: Maybe ByteString -> a
    parseField'Internal = \cases
        Nothing -> error "unexpected null"
        (Just val) -> parseField val

instance FromField Text where
    parseField :: ByteString -> Text
    parseField bs = case valueParser text_strict bs of
        Left e -> error "parse error"
        Right t -> t

instance FromField Bool where
    parseField :: ByteString -> Bool
    parseField bs = case valueParser bool bs of
        Left e -> error "parse error"
        Right t -> t

instance FromField Int where
    parseField :: ByteString -> Int
    parseField bs = case valueParser int bs of
        Left e -> error "parse error"
        Right t -> t

instance (FromField a) => FromField (Maybe a) where
    parseField :: ByteString -> Maybe a
    parseField = error "unexpected maybe"

    parseField'Internal :: (FromField a) => Maybe ByteString -> Maybe a
    parseField'Internal = \cases
        Nothing -> Nothing
        (Just bs) -> Just $ parseField bs
