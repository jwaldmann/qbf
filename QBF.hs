module QBF where

import CNF

data Quantifier = Exists | Forall deriving ( Eq, Ord )

instance Show Quantifier where
    show q = case q of Exists -> "e" ; Forall -> "a"

data Declaration = Declaration Quantifier [Variable] deriving Show

data QBF = QBF [ Declaration ] CNF deriving Show

clauses :: QBF -> Int
clauses (QBF ds (CNF cs)) = length cs

variables :: QBF -> Int
variables (QBF ds cs) = 
    maximum $ do (Declaration q vs) <- ds ; map unVariable vs

