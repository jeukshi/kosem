module Database.Kosem.PostgreSQL.Internal.Classes where

import Data.ByteString.Builder (Builder, stringUtf8)
import Data.Text (Text)
import Data.Text qualified as T

class ToRawSql a where
    toRawSql :: a -> Builder

-- TODO this prolly can be done better?
-- Or maybe we will switch to parsing ByteString instead of Text
-- in the first place.
textToBuilder :: Text -> Builder
textToBuilder = stringUtf8 . T.unpack
