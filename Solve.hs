module Solve where

import CNF
import QBF

import Prelude hiding ( negate )
import qualified Prelude
import Control.Monad ( when, guard )
import qualified Data.Map.Strict as M
import Data.List ( maximumBy, genericLength )
import Data.Function (on)

complete :: QBF -> Bool
complete (QBF ds cnf) = 
    case msat cnf of
        Just result -> result
        Nothing -> case ds of
            Declaration q [] : ds' ->
                 complete (QBF ds' cnf)
            Declaration q (v:vs) : ds' -> 
                ( case q of Forall -> and ; Exists -> or )
              $ do p <- [ False, True ]
                   let d = Declaration q vs
                   return $ complete ( QBF (d:ds') 
                          $ instantiate (literal v p) cnf )

completeIO :: QBF -> IO Bool
completeIO q = cio 0 q

pick_next_variable vs cnf = case vs of
    [] -> Nothing
    v : vs' -> Just (v, vs')

pick_frequent_variable vs cnf = do
    guard $ not $ null vs
    let c :: M.Map Variable Double
        c = M.fromList $ do 
            cl <- cnf ; l <- cl ; return (variable l, 1 / genericLength cl )
    let (v,n) = maximumBy ( compare `on` snd) $ M.toList c
    return v

cio depth (QBF ds cnf) = do
    when (depth < 100) $ print depth
    case msat cnf of
        Just result -> return result
        Nothing -> 
            let Declaration q vs : ds' = ds
            in  case pick_next_variable vs cnf of
                Nothing -> cio depth (QBF ds' cnf)
                Just (v, vs') -> do
                    let d = Declaration q vs'
                    left <- cio (succ depth) 
                        $ QBF (d : ds') 
                        $ instantiate (literal v False) cnf 
                    case (q , left) of
                        (Forall, False) -> return False
                        (Exists, True ) -> return True
                        _ -> do
                            right <- cio (succ depth) 
                                $ QBF (d : ds') 
                                $ instantiate (literal v True) cnf
                            return $ ( case q of Forall -> and ; Exists -> or ) [ left, right ]
