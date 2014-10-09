module Solve where

import CNF
import QBF

import Prelude hiding ( negate )
import qualified Prelude
import Control.Monad ( when )

complete :: QBF -> Bool
complete (QBF ds cnf) = 
    case msat cnf of
        Just result -> result
        Nothing -> case ds of
            Declaration q v : ds' -> 
                ( case q of Forall -> and ; Exists -> or )
              $ do p <- [ False, True ]
                   return $ complete ( QBF ds' $ instantiate (literal v p) cnf )

completeIO :: QBF -> IO Bool
completeIO q = cio 0 q

cio depth (QBF ds cnf) = do
    when (depth < 100) $ print depth
    case msat cnf of
        Just result -> return result
        Nothing -> do
            let Declaration q v : ds' = ds
            left <- cio (succ depth) $ QBF ds' $ instantiate (literal v False) cnf 
            case (q , left) of
                (Forall, False) -> return False
                (Exists, True ) -> return True
                _ -> do
                    right <- cio (succ depth) $ QBF ds' $ instantiate (literal v True) cnf
                    return $ ( case q of Forall -> and ; Exists -> or ) [ left, right ]
