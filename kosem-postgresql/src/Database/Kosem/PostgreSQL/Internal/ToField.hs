-- |

module Database.Kosem.PostgreSQL.Internal.ToField where

import Data.Text (Text)
import Data.ByteString (ByteString)
import PostgreSQL.Binary.Encoding
import Language.Haskell.TH

-- TODO is ToField even a good name?
-- maybe Serialize? ToParam? Param?
-- what about FromField then...
class ToField a where
    toField :: a -> ByteString

instance ToField Text where
    toField :: Text -> ByteString
    toField = encodingBytes . text_strict

instance ToField Bool where
    toField :: Bool -> ByteString
    toField = encodingBytes . bool

instance ToField Int where
    toField :: Int -> ByteString
    toField = encodingBytes . int8_int64 . fromIntegral

instance ToField a => ToField (Maybe a) where
    toField :: ToField a => Maybe a -> ByteString
    toField = \cases
      -- TODO this doesn't look good
      Nothing -> encodingBytes $ text_strict "null"
      (Just x) -> toField x
