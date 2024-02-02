{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Kosem.PostgreSQL.Internal.FromField where

import Data.Text (Text)
import Data.ByteString (ByteString)
import PostgreSQL.Binary.Decoding
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

class FromField a where
    fromField :: Text -> a

class SqlType a where
    parseField :: Maybe ByteString -> a

    unsafeAny :: a -> Any
    unsafeAny = unsafeCoerce

instance SqlType Text where
    parseField = \cases
      Nothing -> error "nothing"
      (Just bs) -> case valueParser text_strict bs of
        Left e -> error "parse error"
        Right t -> t

instance SqlType a => SqlType (Maybe a) where
    parseField = \cases
      Nothing -> Nothing
      justBs -> parseField justBs
        -- Left e -> error "parse error"
        -- Right t -> Just t



class SqlType sql => FromSql sql haskell | haskell -> sql where
   fromSql :: sql -> haskell

instance FromSql sql a => FromSql sql (Maybe a) where

instance FromSql Text Text where
  fromSql = id
