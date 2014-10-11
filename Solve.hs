module Solve where

import CNF
import QBF

import qualified System.Console.ANSI as A
import System.IO

import Prelude hiding ( negate )
import qualified Prelude
import Control.Monad ( when, guard )
import qualified Data.Map.Strict as M
import Data.List ( maximumBy, genericLength )
import Data.Function (on)
import Control.Concurrent.STM


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
completeIO q = do
    loc <- atomically $ newTVar 0
    cio loc 0 q

cio loc depth (QBF ds cnf) = do
    -- when (depth < 10) $ print depth
    case msat cnf of
        Just result -> return result
        Nothing -> 
            let Declaration q vs : ds' = ds
            in  case pick_next_variable vs cnf of
                Nothing -> cio loc depth (QBF ds' cnf)
                Just (v, f, vs') -> do
                    let d = Declaration q vs'
                    trace loc depth q v f
                    left <- cio loc (succ depth) 
                        $ QBF (d : ds') 
                        $ instantiate (literal v f) cnf 
                    case (q , left) of
                        (Forall, False) -> return False
                        (Exists, True ) -> return True
                        _ -> do
                            trace loc depth q v $ not f
                            right <- cio loc (succ depth) 
                                $ QBF (d : ds') 
                                $ instantiate (literal v $ not f) cnf
                            return $ ( case q of Forall -> and ; Exists -> or ) [ left, right ]

trace loc depth q v f = when (depth < 80) $ do
    click <- atomically $ do
        c <- readTVar loc
        writeTVar loc $ mod (succ c) $ 10 ^ 5 
        return $ c == 0
    when click $ do
        A.setCursorColumn $ 1 * depth 
        let col = case f of False -> A.Blue ; True -> A.Red
        A.setSGR [ A.SetColor A.Foreground A.Dull col ]
        putStr $ show q -- ++ show v 
        hFlush stdout


pick_next_variable vs cnf = case vs of
    [] -> Nothing
    v : vs' -> Just (v, False, vs')

pick_frequent_variable vs cnf = do
    guard $ not $ null vs
    let c :: M.Map Literal Double
        c = M.fromList $ do 
            cl <- cnf ; l <- cl ; return (l, 1 / 2 ^ genericLength cl )
    let (l,n) = maximumBy ( compare `on` snd) $ M.toList c
    return ( variable l, not $ polarity l, filter (/= variable l) vs )

