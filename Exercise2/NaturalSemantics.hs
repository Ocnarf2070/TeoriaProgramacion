{-|

Programming Languages
Fall 2018

Implementation in Haskell of the Natural Semantics described in Chapter 2 of
Nielson & Nielson, Semantics with Applications

Authors: Hasnaouia Meskini Iman, 
		González Sánchez Franco Emanuel 

-}

module NaturalSemantics where

import           While
import 			 Exercises01

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s

-- representation of the transition relation <S, s> -> s'

nsStm :: Config -> Config

-- x := a

nsStm (Inter (Ass x a) s)      = Final (update s (x :=>: v))
	where 
		v = aVal' a s

-- skip

nsStm (Inter Skip s)           = Final s

-- s1; s2

nsStm (Inter (Comp ss1 ss2) s) = Final s''
	where
		Final s' = nsStm (Inter ss1 s)
		Final s'' = nsStm (Inter ss2 s')

-- if b then s1 else s2

-- B[b]s = tt
nsStm (Inter (If b ss1 ss2) s) 
	| bVal' b s = Final s'
		where
			Final s' = nsStm (Inter ss1 s)

-- B[b]s = ff
nsStm (Inter (If b ss1 ss2) s)  
	| not $ bVal' b s = Final s'
		where
			Final s' = nsStm (Inter ss2 s)

-- while b do s

-- B[b]s = ff
nsStm (Inter (While b ss) s) 
	| not $ bVal' b s = Final s
	
-- B[b]s = tt
nsStm (Inter (While b ss) s)
	| bVal' b s = Final s''
		where
			Final s' = nsStm (Inter ss s)
			Final s'' =  nsStm (Inter (While b ss) s') 

-- repeat S until b

--B[b] s = ff
nsStm (Inter (Repeat ss b) s) 
	| not $ bVal' b s' = Final s''
		where 
			Final s' = nsStm (Inter ss s)
			Final s'' = nsStm (Inter (Repeat ss b) s')

--B[b] s = tt
nsStm (Inter (Repeat ss b) s) 
	| bVal' b s' = Final s'
		where 
			Final s' = nsStm (Inter ss s)

	
-- for x:= a1 to a2 do S

-- x!=a2
nsStm (Inter (For (Ass v a1) a2 ss) s)
	| not $ bVal' b s = Final s'''
		where
			a1' = Add a1 (N 1)
			b = Eq a1 a2
			Final s' = nsStm (Inter (Ass v a1) s)
			Final s'' = nsStm (Inter ss s')
			Final s''' = nsStm (Inter (For (Ass v a1') a2 ss) s'')

--x==a2
nsStm (Inter (For (Ass v a1) a2 ss) s)
	| bVal' b s = Final s'
		where
			Final s' = nsStm (Inter (Ass v a1) s)
			b = Eq a1 a2

-- semantic function for natural semantics
sNs :: Stm -> State -> State
sNs ss s = s'
  where Final s' = nsStm (Inter ss s)

-- Example C.1
sFac :: State
sFac = sNs factorial sInit
-- End Example C.1
