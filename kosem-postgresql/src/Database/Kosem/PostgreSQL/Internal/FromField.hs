{-# LANGUAGE DefaultSignatures #-}

module Database.Kosem.PostgreSQL.Internal.FromField where

import Data.ByteString (ByteString)
import Data.Int (Int8)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Exts (Any)
import GHC.Int (Int16, Int32, Int64)
import PostgreSQL.Binary.Decoding
import Unsafe.Coerce (unsafeCoerce)

class FromField a where
    -- TODO We should prolly return Either
    -- and allow users to signal failure that way. TBD
    parseField :: ByteString -> a
    {-# MINIMAL parseField | parseFieldWithLen #-}

    default parseField :: ByteString -> a
    parseField = error "TODO nice error"

    parseFieldWithLen :: Int8 -> ByteString -> a
    default parseFieldWithLen :: Int8 -> ByteString -> a
    parseFieldWithLen _ = parseField

    parseField'Internal :: Maybe ByteString -> a
    default parseField'Internal :: Maybe ByteString -> a
    parseField'Internal = \cases
        Nothing -> error "unexpected null"
        (Just val) -> parseField val

    parseFieldWithLen'Internal :: Int8 -> Maybe ByteString -> a
    default parseFieldWithLen'Internal :: Int8 -> Maybe ByteString -> a
    parseFieldWithLen'Internal n = \cases
        Nothing -> error "unexpected null"
        (Just val) -> parseFieldWithLen n val

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
    parseFieldWithLen :: Int8 -> ByteString -> Int
    parseFieldWithLen 8 bs = case valueParser (int @Int64) bs of
        Left e -> error "parse error"
        Right t -> fromIntegral t
    parseFieldWithLen 4 bs = case valueParser (int @Int32) bs of
        Left e -> error "parse error"
        Right t -> fromIntegral t
    parseFieldWithLen 2 bs = case valueParser (int @Int16) bs of
        Left e -> error "parse error"
        Right t -> fromIntegral t
    parseFieldWithLen n _ = error $ "TODO: not implemented for length: " <> show n

instance FromField Float where
    parseField :: ByteString -> Float
    parseField bs = case valueParser float4 bs of
        Left e -> error "parse error"
        Right t -> t

instance FromField Double where
    parseField :: ByteString -> Double
    parseField bs = case valueParser float8 bs of
        Left e -> error "parse error"
        Right t -> t

instance FromField Scientific where
    parseField bs = case valueParser numeric bs of
        Left e -> error "parse error"
        Right t -> t

instance (FromField a) => FromField (Maybe a) where
    parseField :: ByteString -> Maybe a
    parseField = error "unexpected maybe"

    parseField'Internal :: (FromField a) => Maybe ByteString -> Maybe a
    parseField'Internal = \cases
        Nothing -> Nothing
        (Just bs) -> Just $ parseField bs

    parseFieldWithLen'Internal :: (FromField a) => Int8 -> Maybe ByteString -> Maybe a
    parseFieldWithLen'Internal n = \cases
        Nothing -> Nothing
        (Just bs) -> Just $ parseFieldWithLen n bs
