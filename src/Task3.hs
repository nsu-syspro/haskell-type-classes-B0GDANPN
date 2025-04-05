{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- The above pragma enables all warnings

module Task3 where
import Task1 (Parse(..))
import Task2 (Expr(..), Eval(..),evalExpr, Reify)
import Data.List (nub)

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
--
data BoolOp = And | Or | Xor
  deriving (Show, Eq)

instance Parse BoolOp where
  parse "and" = Just And
  parse "or"  = Just Or
  parse "xor" = Just Xor
  parse _     = Nothing

instance Eval Bool BoolOp where
  evalBinOp And = (&&)
  evalBinOp Or  = (||)
  evalBinOp Xor = (/=)


solveSAT :: String -> Maybe Bool
solveSAT input = do
  expr <- parse input :: Maybe (Expr Bool BoolOp)
  let vars = getVariables expr
  let combinations = generateCombinations vars
  anyM (checkCombination expr) combinations

getVariables :: Expr a op -> [String]
getVariables (Lit _)    = []
getVariables (Var v)    = [v]
getVariables (BinOp _ l r) = nub $ getVariables l ++ getVariables r


generateCombinations :: [String] -> [[(String, Bool)]]
generateCombinations vars = sequence [ [(v, False), (v, True)] | v <- vars ]


checkCombination :: Expr Bool BoolOp -> [(String, Bool)] -> Maybe Bool
checkCombination expr env = do
  evalExpr env expr


anyM :: (a -> Maybe Bool) -> [a] -> Maybe Bool
anyM _ [] = Just False
anyM f (x:xs) = do
  b <- f x
  if b then Just True else anyM f xs

reifyBool :: Reify Bool BoolOp
reifyBool = id
