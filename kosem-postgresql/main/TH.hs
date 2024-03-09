{-# LANGUAGE TemplateHaskell #-}

module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Quasi(..))

checkType :: String -> Q Exp
checkType varNameStr = do
    varName <- lookupValueName varNameStr
    -- varName <- qLookupName False varNameStr
    -- varName <- lookupTypeName varNameStr
    -- ty <- lookupValueName varNameStr
    case varName of
        Just name -> do
            -- Obtain information about the variable
            -- VarI _ varType _ <- reify name
            info <- reify name
            -- Generate an expression that returns the type
            return $ LitE $ StringL $ show info
        Nothing -> fail $ "Variable not found: " ++ varNameStr


tc :: QuasiQuoter
tc = QuasiQuoter
    { quoteExp  = checkType
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

z :: Int
z = 9

-- Function to extract the type of a variable
extractType :: Name -> Q Type
extractType varName = do
    -- Get the type signature of the variable
    VarI _ varType _ <- reify varName
    return varType

-- Quasiquote to capture the type of a variable
typeOf :: QuasiQuoter
typeOf = QuasiQuoter
    { quoteExp  = \varNameStr -> do
                    let varName = mkName varNameStr
                    varType <- extractType varName
                    return $ LitE $ StringL $ pprint varType
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

class ToSql a where
  toSql :: a -> String

instance ToSql Int where
  toSql :: Int -> String
  toSql = show

instance ToSql a => ToSql [a] where
  toSql _ = "abc"
