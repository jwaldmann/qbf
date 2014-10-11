-- | very simple-minded implementation, just showing the API,
-- implementation needs to be improved for real work.

module CNF where

import Prelude hiding ( negate )
import qualified Prelude

newtype Variable = Variable { unVariable :: Int }
    deriving (Eq, Ord)

instance Show Variable where show (Variable v) = show v

newtype Literal = Literal { unLiteral :: Int } 
    deriving (Eq, Ord)

instance Show Literal where show (Literal l) = show l

literal :: Variable -> Bool -> Literal
literal (Variable v) p = 
    if p then Literal v else Literal $ Prelude.negate v

polarity :: Literal -> Bool
polarity (Literal l) = l > 0

negate :: Literal -> Literal
negate (Literal l) = Literal (Prelude.negate l)

data CNF = CNF [[ Literal ]] deriving Show

instantiate :: Literal -> CNF -> CNF
instantiate l (CNF clauses) = 
    CNF $ map ( filter ( \ l' -> negate l /= l' ) ) -- these literals are false
        $ filter ( \ clause ->  not (elem l clause) ) -- these clauses are true
        $ clauses

-- | detect trivial cases of satisfiability
msat :: CNF -> Maybe Bool
msat (CNF clauses) = 
    if null clauses then Just True
    else if elem [] clauses then Just False
    else Nothing

