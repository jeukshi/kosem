{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoDuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Sql.Types where

import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.P (P)
import Database.Kosem.PostgreSQL.Internal.Types (
    HsIdentifier,
    Identifier,
    IsNullable,
    PgType,
    SqlMapping,
 )
import GHC.Exts (Any)
import Language.Haskell.TH.Lift (Lift)
import Language.Haskell.TH.Syntax (Name)

-- TODO type param `fetch` (One/Many)
-- TODO type para `database` - database token
data SqlCommand result = SqlCommand
    { statement :: ByteString
    , columnsNumber :: Int
    , rowProto :: result
    , rowParser :: [Maybe ByteString -> Any]
    , params :: [Maybe ByteString]
    }

-- type CommandInput = [(Identifier, Name, IsNullable)]

data CommandInfo a = CommandInfo
    { output :: NonEmpty a
    , input :: [CommandInput]
    , rawCommand :: String
    }
    deriving (Show)

parameterTypeToText :: ParameterType -> Text
parameterTypeToText = \case
    SimpleParameter -> ":"
    SimpleMaybeParameter -> ":?"

guardTypeToText :: GuardType -> Text
guardTypeToText = \case
    BooleanGuard -> ":"
    MaybeGuard -> ":?"

data CommandOutput = MkCommandOutput
    { coIdentifier :: Identifier
    , coPgType :: PgType
    , coNullable :: IsNullable
    }
    deriving (Show)

data CommandInput
    = CommandParameter Parameter
    | CommandGuard Guard
    deriving (Show)

commandInputPosition :: CommandInput -> P
commandInputPosition = \cases
    (CommandParameter p) -> p.position
    (CommandGuard g) -> g.guardPos

data Guard = Guard
    { guardPos :: P
    , openBracketPos :: P
    , closeBracketPos :: P
    , gIdentifier :: HsIdentifier
    , guardType :: GuardType
    }
    deriving (Show)

data GuardType
    = BooleanGuard
    | MaybeGuard
    deriving (Show, Eq)

data Parameter = Parameter
    { position :: P
    , pIdentifier :: HsIdentifier
    , paramType :: ParameterType
    , pPgType :: PgType
    , pNullable :: IsNullable
    }
    deriving (Show)

data ParameterType
    = SimpleParameter
    | SimpleMaybeParameter
    deriving (Show, Eq)

data CommandParameter = MkCommandParameter
    { cpIdentifier :: HsIdentifier
    , cpHsType :: Name
    , cpIsNullable :: IsNullable
    }
    deriving (Show)

data ParameterInfo = ParameterInfo
    { pgType :: PgType
    , hsType :: Name
    , nullable :: IsNullable
    }
    deriving (Show)

data Choice = Choice
    { choiceIdentifier :: HsIdentifier
    , choiceOption :: ChoiceOption
    }
    deriving (Show)

data ChoiceOption
    = VcBool Bool
    | VcMaybe Bool
    deriving (Show)

data Command a b = MkCommand
    { cParams :: a
    , cCommand :: b
    }
    deriving (Show, Functor, Foldable, Traversable)

instance Bifunctor Command where
    bimap f g (MkCommand a b) =
        MkCommand (f a) (g b)

data CommandVariant a b
    = SingleCommand (Command a b)
    | TwoCommands
        HsIdentifier
        ChoiceOption
        (Command a b)
        ChoiceOption
        (Command a b)
    | MultipleCommands [([Choice], Command a b)]
    deriving (Show, Functor, Foldable, Traversable)

instance Bifunctor CommandVariant where
    bimap f g (SingleCommand command) =
        SingleCommand (bimap f g command)
    bimap f g (TwoCommands identifier opt1 cmd1 opt2 cmd2) =
        TwoCommands identifier opt1 (bimap f g cmd1) opt2 (bimap f g cmd2)
    bimap f g (MultipleCommands commands) =
        MultipleCommands (map (second (bimap f g)) commands)
