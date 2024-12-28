{-# LANGUAGE PatternSynonyms #-}

module Database.Kosem.PostgreSQL.Internal.TypCategory where

import Language.Haskell.TH.Lift (Lift)

newtype TypCategory = TypCategory Char
    deriving (Show, Eq, Lift)

pattern A :: TypCategory
pattern A <- TypCategory 'A'
    where
        A = TypCategory 'A' -- 'A': Array types

pattern B :: TypCategory
pattern B <- TypCategory 'B'
    where
        B = TypCategory 'B' -- 'B': Boolean types

pattern C :: TypCategory
pattern C <- TypCategory 'C'
    where
        C = TypCategory 'C' -- 'C': Composite types

pattern D :: TypCategory
pattern D <- TypCategory 'D'
    where
        D = TypCategory 'D' -- 'D': Date/time types

pattern E :: TypCategory
pattern E <- TypCategory 'E'
    where
        E = TypCategory 'E' -- 'E': Enum types

pattern G :: TypCategory
pattern G <- TypCategory 'G'
    where
        G = TypCategory 'G' -- 'G': Geometric types

pattern I :: TypCategory
pattern I <- TypCategory 'I'
    where
        I = TypCategory 'I' -- 'I': Network address types

pattern N :: TypCategory
pattern N <- TypCategory 'N'
    where
        N = TypCategory 'N' -- 'N': Numeric types

pattern P :: TypCategory
pattern P <- TypCategory 'P'
    where
        P = TypCategory 'P' -- 'P': Pseudo types

pattern R :: TypCategory
pattern R <- TypCategory 'R'
    where
        R = TypCategory 'R' -- 'R': Range types

pattern S :: TypCategory
pattern S <- TypCategory 'S'
    where
        S = TypCategory 'S' -- 'S': String types

pattern T :: TypCategory
pattern T <- TypCategory 'T'
    where
        T = TypCategory 'T' -- 'T': Timespan types

pattern U :: TypCategory
pattern U <- TypCategory 'U'
    where
        U = TypCategory 'U' -- 'U': User-defined types

pattern V :: TypCategory
pattern V <- TypCategory 'V'
    where
        V = TypCategory 'V' -- 'V': Bit-string types

pattern X :: TypCategory
pattern X <- TypCategory 'X'
    where
        X = TypCategory 'X' -- 'X': Unknown types

pattern Z :: TypCategory
pattern Z <- TypCategory 'X'
    where
        Z = TypCategory 'Z' -- 'Z': Internal-use types

{- | This is just an arbitrary number to compare priority
https://www.postgresql.org/docs/current/catalog-pg-type.html#CATALOG-TYPCATEGORY-TABLE
-}
typCategoryPriority :: TypCategory -> Int
typCategoryPriority = \case
    A -> 10
    B -> 20
    C -> 30
    D -> 40
    E -> 50
    G -> 60
    I -> 70
    N -> 80
    P -> 90
    R -> 100
    S -> 110
    T -> 120
    U -> 130
    V -> 140
    X -> 150
    Z -> 160
    _ -> 999
