{-|

Programming Languages
Fall 2018

Implementation in Haskell of the Structural Operational Semantics
described in Chapter 2 of Nielson & Nielson, Semantics with Applications

Author:Hasnaouia Meskini Iman,
		González Sánchez Franco Emanuel

-}

module StructuralSemantics where

import           ExWhile

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s
						| Stuck Stm State      -- s

isFinal :: Config -> Bool
isFinal (Inter ss s)		= False
isFinal (Final s)   		= True
isFinal (Stuck ss s)    = False

isStuck :: Config -> Bool
isStuck (Stuck stm s) = True --Una configuracion stuck siempre va a ser intermedia
isStuck _ = False

-- representation of the transition relation <S, s> -> s'

sosStm :: Config -> Config

-- x := a

sosStm (Inter (Ass x a) s) = Final (update s x (aVal a s))
  where
    update s x v y = if x == y then v else s y

-- skip

sosStm (Inter Skip s) = Final s

-- s1; s2

sosStm (Inter (Comp ss1 ss2) s)
	|isStuck conf  = Stuck ss1 s
		where
			conf = sosStm (Inter ss1 s)
			
sosStm (Inter (Comp ss1 ss2) s)
	|isFinal conf  = Inter ss2 s'
		where
			conf = sosStm (Inter ss1 s)
			Final s' = conf
sosStm (Inter (Comp ss1 ss2) s)
	|otherwise = Inter (Comp ss1' ss2) s'
		where Inter ss1' s' = sosStm (Inter ss1 s)

-- if b then s1 else s2


sosStm (Inter (If b ss1 ss2) s)
	--if-tt
	|bVal b s = Inter ss1 s
	--if-ff
--	|not $ bVal' b s = sosStm (Inter ss2 s)
	|otherwise = Inter ss2 s


-- while b do s

sosStm (Inter (While b ss) s) = Inter (If b ss1 Skip) s
	where
		ss1 = Comp ss (While b ss)

-- repeat S until b
sosStm (Inter (Repeat ss b) s) = Inter (Comp ss ss2) s
	where
		ss2 = If (Neg b) ss1 Skip
		ss1 = Repeat ss b


-- abort

sosStm (Inter Abort s) =  Stuck Abort s

-- todo
