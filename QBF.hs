module QBF where

import CNF

data Quantifier = Exists | Forall deriving ( Eq, Ord, Show )

data Declaration = Declaration Quantifier Variable deriving Show

data QBF = QBF [ Declaration ] CNF deriving Show


