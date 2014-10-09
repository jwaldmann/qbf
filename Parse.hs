module Parse where

import CNF
import QBF

import Prelude hiding ( negate )
import qualified Prelude

import Text.Parsec.ByteString
import Text.Parsec
import Text.Parsec.Char

import Control.Applicative ((<$>))

read_qdimacs f = do
    res <- parseFromFile qbf f
    case res of
        Left err -> error $ show err
        Right q -> return q

qbf :: Parser QBF
qbf = do
    many comment
    string "p" ; spaces
    string "cnf" ; spaces
    nv <- natural ; nc <- natural
    p <- prefix
    c <- cnf
    return $ QBF p c

comment = do
    string "c"
    manyTill anyChar newline
    spaces

prefix = concat <$> many prefix_line

prefix_line = do
    q <- quantifier
    vs <- many variable ; zero
    return $ map (Declaration q) vs
    
quantifier 
     =  do string "a" ; spaces ; return Forall
    <|> do string "e" ; spaces ; return Exists

variable = try $ do
    n <- natural
    if n > 0 then return $ Variable n else fail "variable"

cnf = CNF <$> many clause

clause = do
    ls <- many Parse.literal ; zero
    return ls

literal = try $ do
    p <- option True (do string "-"; return False) 
    v <- variable
    return $ CNF.literal v p

natural :: Parser Int
natural = do
    ds <- many1 digit ; spaces
    return $ foldl ( \ a d -> 10 * a + fromEnum d - fromEnum '0' ) 0 ds

zero = do string "0" ; spaces
