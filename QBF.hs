module QBF where

import CNF

data Quantifier = Exists | Forall deriving ( Eq, Ord, Show )

data Declaration = Declaration Quantifier Variable deriving Show

data QBF = QBF [ Declaration ] CNF deriving Show

clauses :: QBF -> Int
clauses (QBF ds (CNF cs)) = length cs

variables :: QBF -> Int
variables (QBF ds cs) = 
    maximum $ do (Declaration q (Variable v)) <- ds ; return v

