--Sintaxis abstracta de Bin
module Binario where

data Bin = Zero | One | FZero Bin | FOne Bin deriving Eq

instance Show Bin where
	show Zero = "0"
	show One = "1"
	show (FZero n) = show n ++ "0" 
	show (FOne n) = show n ++ "1"

num :: Bin
num = FOne (FOne (FZero One))

eval :: Bin -> Integer
eval Zero = 0
eval One = 1
eval (FZero n) = 2 * eval n 
eval (FOne n) = 2 * eval n + 1