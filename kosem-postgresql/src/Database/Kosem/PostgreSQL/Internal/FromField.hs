{-# LANGUAGE UndecidableInstances #-}

module Database.Kosem.PostgreSQL.Internal.FromField where

import Data.Text (Text)
import Data.ByteString (ByteString)
import PostgreSQL.Binary.Decoding
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

class FromField a where
    parseField :: Maybe ByteString -> a

instance FromField Text where
    parseField :: Maybe ByteString -> Text
    parseField = \cases
      Nothing -> error "nothing"
      (Just bs) -> case valueParser text_strict bs of
        Left e -> error "parse error"
        Right t -> t

instance FromField Bool where
    parseField :: Maybe ByteString -> Bool
    parseField = \cases
      Nothing -> error "nothing"
      (Just bs) -> case valueParser bool bs of
        Left e -> error "parse error"
        Right t -> t

instance FromField Int where
    parseField :: Maybe ByteString -> Int
    parseField = \cases
      Nothing -> error "nothing"
      (Just bs) -> case valueParser int bs of
        Left e -> error "parse error"
        Right t -> t

instance FromField a => FromField (Maybe a) where
    parseField :: FromField a => Maybe ByteString -> Maybe a
    parseField = \cases
      Nothing -> Nothing
      justBs -> parseField justBs
