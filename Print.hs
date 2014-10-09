module Print where

import QBF
import CNF

show_qdimacs :: QBF -> String
show_qdimacs q @ (QBF ds (CNF cs)) = unlines $
       [ unwords [ "p", "cnf", show $ variables q, show $ clauses q ] ]
    ++ ( do Declaration q (Variable v) <- ds ; 
            return $ unwords [ case q of Forall -> "a" ; Exists -> "e" 
                             , show v
                             , "0"
                             ]
       )
    ++ ( do c <- cs
            return $ unwords $ map ( \ (Literal l) -> show l ) c ++ [ "0" ] )

        
