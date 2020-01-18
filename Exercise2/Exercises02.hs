{-|

Programming Languages
Fall 2018

Implementation in Haskell of the Natural Semantics described in Chapter 2 of
Nielson & Nielson, Semantics with Applications

Authors: Hasnaouia Meskini Iman, 
		González Sánchez Franco Emanuel 

-}

module Exercises02 where

import           Exercises01      (Update (..), fvAexp, fvBexp, update)
import           NaturalSemantics
import           Test.HUnit       hiding (State)
import           While
import 			 Data.List

-- |----------------------------------------------------------------------
-- | Exercise 1
-- |----------------------------------------------------------------------
-- | The function 'sNs' returns the final state of the execution of a
-- | WHILE statement 'st' from a given initial state 's'. For example:
-- |
-- |  sNs factorial sInit
-- |
-- | returns the final state:
-- |
-- |    s x = 1
-- |    s y = 6
-- |    s _ = 0
-- |
-- | Since a state is a function it cannot be printed thus you cannot
-- | add 'deriving Show' to the algebraic data type 'Config'.
-- | The goal of this exercise is to define a number of functions to
-- | "show" a state thus you can inspect the final state computed by the
-- | natural semantics of WHILE.

-- | Exercise 1.1
-- | Define a function 'showState' that given a state 's' and a list
-- | of variables 'vs' returns a list of strings showing the bindings
-- | of the variables mentioned in 'vs'. For example, for the state
-- | 's' above we get:
-- |
-- |    showState s ["x"] = ["x -> 1"]
-- |    showState s ["y"] = ["y -> 6"]
-- |    showState s ["x", "y"] = ["x -> 1", "y -> 6"]
-- |    showState s ["y", "z", "x"] = ["y -> 6", "z -> 0", "x -> 1"]

showState :: State -> [Var] -> [String]
showState s [] = []
showState s (x:xs) = [x ++ " -> " ++ (show $ s x)] ++ showState s xs 

-- | Test your function with HUnit.


-- | Exercise 1.2
-- | Define a function 'fvStm' that returns the free variables of a WHILE
-- | statement. For example:
-- |
-- | fvStm factorial = ["y","x"]
-- |
-- | Note: the order of appearance is not relevant, but there should not be
-- | duplicates.

fvStm :: Stm -> [Var]
fvStm (Ass v aexp) = nub $ fvAexp (V v) ++ fvAexp aexp
fvStm Skip = []
fvStm (Comp stm1 stm2) = nub $ fvStm stm1 ++ fvStm stm2
fvStm (If bexp stm1 stm2) = nub $ fvBexp bexp ++ fvStm stm1 ++ fvStm stm2
fvStm (While bexp stm) = nub $ fvBexp bexp ++ fvStm stm
fvStm (Repeat stm bexp) = nub $ fvStm stm ++ fvBexp bexp 
fvStm (For (Ass v a1) a2 stm) = nub $ fvAexp (V v) ++ fvAexp a1 ++ fvAexp a2 ++ fvStm stm 

-- | Test your function with HUnit. Beware the order or appearance.


-- | Exercise 1.3
-- | Define a function 'showFinalState' that given a WHILE statement and a
-- | initial state returns a list of strings with the bindings of
-- | the free variables of the statement in the final state. For
-- | example:
-- |
-- |  showFinalState factorial sInit = ["y->6","x->1"]

showFinalState :: Stm -> State -> [String]
showFinalState st s = showState (sNs st s) $ fvStm st

-- | Test your function with HUnit. Beware the order or appearance.


-- |----------------------------------------------------------------------
-- | Exercise 2
-- |----------------------------------------------------------------------
-- | Write a program in WHILE to compute z = x^y and check it by obtaining a
-- | number of final states.

power x y = Comp inicial (While b stm) -- WHILE statement to compute z = x^y
	where 
		inicial = Comp (Ass "x" (N x)) (Comp (Ass "y" (N y)) (Comp (Ass "z" (N 1)) (Ass "i" (N 0))))
		b = Neg (Eq (V "i") (V "y"))
		stm = Comp (Ass "z" (Mult (V "z") (V "x"))) (Ass "i" (Add (V "i") (N 1)))

-- power (x1,y1)
-- x=x1; y=y1; z=1; i=0;
-- While (i != y){
--	z = z*x;
--  i=i+1;
--}

-- | Test your function with HUnit. Inspect the final states of at least
-- | four different executions.


-- |----------------------------------------------------------------------
-- | Exercise 3
-- |----------------------------------------------------------------------
-- | The WHILE language can be extended with a 'repeat S until b' construct.

-- | Exercise 3.1
-- | Define the natural semantics of this new construct. You are not allowed
-- | to rely on the 'while b do S' statement.

{- Formal definition of 'repeat S until b'

					 <S,s> -> s' <repeat S until b,s'> -> s''
        [repeat-ff]  -------------------------------------------- B[b]s = ff
							<repeat S until b,s> -> s''


							<S,s> -> s'
        [repeat-tt]  ------------------------------   B[b]s = tt
					   <repeat S until b,s> -> s'

-}

-- | Extend  the definitions of  data type 'Stm' (in  module While.hs)
-- |  and  'nsStm'  (in  module NaturalSemantics.hs)  to  include  the
-- | 'repeat  S until b' construct.  Write a couple of  WHILE programs
-- | that use the 'repeat' statement and test your functions with HUnit.

rep = Comp inicial cuerpo
	where
		inicial = Comp (Comp (Ass "x" (N 9)) (Ass "y" (N 1))) (Ass "z" (N 0))
		cuerpo = Repeat ss b
		b=Eq (V "x") (V "y")
		ss = Comp (Ass "z" (Add (V "z") (Mult (V "x") (V "y")))) (Ass "y" (Add (V "y") (N 1)))

--x=9;y=1;z=0;
--repeat 
--z=z+x*y;
--y=y+1;
--until (x==y)



-- |----------------------------------------------------------------------
-- | Exercise 4
-- |----------------------------------------------------------------------
-- | The WHILE language can be extended with a 'for x:= a1 to a2 do S'
-- | construct.

-- | Exercise 4.1
-- | Define the natural semantics of this new construct. You are not allowed
-- | to rely on the 'while b do S' or the 'repeat S until b' statements.

{- Formal definition of 'for x:= a1 to a2 do S'


-}

-- | Extend  the definitions of  data type 'Stm' (in  module While.hs)
-- | and  'nsStm'  (in  module NaturalSemantics.hs)  to  include  the
-- | 'for x:= a1 to a2 do S' construct.  Write a couple of  WHILE programs
-- | that use the 'for' statement and test your functions with HUnit.

for = Comp inicial cuerpo
	where
		inicial = Comp (Ass "y" (N 9)) (Ass "z" (N 0))
		cuerpo = For (Ass "x" (N 1)) (V "y") ss
		ss = Ass "z" (Add (V "z") (Mult (V "x") (V "y")))
--y=9;z=0;
--for x=1 to y do{
--z=z+x*y;
--}

-- |----------------------------------------------------------------------
-- | Exercise 5
-- |----------------------------------------------------------------------

-- | Define the semantics of arithmetic expressions (Aexp) by means of
-- | natural semantics. To that end, define an algebraic datatype 'ConfigAexp'
-- | to represent the configurations, and a function 'nsAexp' to represent
-- | the transition relation.

-- representation of configurations for Aexp, (replace TODO by appropriate
-- data definition)

data ConfigAExp = InterAexp Aexp State  -- <a, s>
				| FinalAexp Z	        -- z

-- representation of the transition relation <A, s> -> s'

nsAexp :: ConfigAExp -> ConfigAExp
nsAexp (InterAexp (N n) s) = FinalAexp n
nsAexp (InterAexp (V v) s) = FinalAexp (s v)
nsAexp (InterAexp (Add a1 a2) s) = FinalAexp z
	where
		FinalAexp z1 = nsAexp (InterAexp a1 s)
		FinalAexp z2 = nsAexp (InterAexp a2 s)
		z = z1 + z2
nsAexp (InterAexp (Mult a1 a2) s) = FinalAexp z
	where
		FinalAexp z1 = nsAexp (InterAexp a1 s)
		FinalAexp z2 = nsAexp (InterAexp a2 s)
		z = z1 * z2
nsAexp (InterAexp (Sub a1 a2) s) = FinalAexp z
	where
		FinalAexp z1 = nsAexp (InterAexp a1 s)
		FinalAexp z2 = nsAexp (InterAexp a2 s)
		z = z1 - z2

showNsAexp :: ConfigAExp -> Z
showNsAexp s = s'
	where 
		FinalAexp s' = nsAexp s
-- | Test your function with HUnit. Inspect the final states of at least
-- | four different evaluations.


----------------------

data ConfigBExp = InterBexp Bexp State  -- <b, s>
				| FinalBexp T	        -- t

-- representation of the transition relation <B, s> -> s'

nsBexp :: ConfigBExp -> ConfigBExp
nsBexp (InterBexp TRUE s) = FinalBexp True
nsBexp (InterBexp FALSE s) = FinalBexp False
nsBexp (InterBexp (Eq a1 a2) s) = FinalBexp t
	where
		FinalAexp z1 = nsAexp (InterAexp a1 s)
		FinalAexp z2 = nsAexp (InterAexp a2 s)
		t = z1 == z2
nsBexp (InterBexp (Le a1 a2) s) = FinalBexp t
	where
		FinalAexp z1 = nsAexp (InterAexp a1 s)
		FinalAexp z2 = nsAexp (InterAexp a2 s)
		t = z1 <= z2
nsBexp (InterBexp (Neg b) s) = FinalBexp (not t)
	where
		FinalBexp t = nsBexp (InterBexp b s)
nsBexp (InterBexp (And b1 b2) s) = FinalBexp t
	where
		FinalBexp t1 = nsBexp (InterBexp b1 s)
		FinalBexp t2 = nsBexp (InterBexp b2 s)
		t = t1 && t2

	
showNsBexp :: ConfigBExp -> T
showNsBexp t = t'
	where 
		FinalBexp t' = nsBexp t

-- |----------------------------------------------------------------------
-- | Exercise 6
-- |----------------------------------------------------------------------

-- | Given the algebraic data type 'DerivTree' to represent derivation trees
-- | of the natural semantics:

data Transition = Config :-->: State

data DerivTree = AssNS     Transition
               | SkipNS    Transition
               | CompNS    Transition DerivTree DerivTree
               | IfTTNS    Transition DerivTree
               | IfFFNS    Transition DerivTree
               | WhileTTNS Transition DerivTree DerivTree
               | WhileFFNS Transition

-- | and the function 'getFinalState' to access the final state of the root
-- | of a derivation tree:

getFinalState :: DerivTree -> State
getFinalState (AssNS  (_ :-->: s))         = s
getFinalState (SkipNS (_ :-->: s))         = s
getFinalState (CompNS (_ :-->: s) _ _ )    = s
getFinalState (IfTTNS (_ :-->: s) _ )      = s
getFinalState (IfFFNS (_ :-->: s) _ )      = s
getFinalState (WhileTTNS (_ :-->: s) _ _ ) = s
getFinalState (WhileFFNS (_ :-->: s))      = s

-- | Define a function 'nsDeriv' that given a WHILE statement 'st' and an
-- | initial state 's' returns corresponding derivation tree.

nsDeriv :: Stm -> State -> DerivTree
nsDeriv (Ass v aexp) s = AssNS (conf :-->: state)
	where
		conf = Inter (Ass v aexp) s
		Final state = nsStm conf
nsDeriv Skip s = SkipNS (conf :-->: state)
	where
		conf = Inter Skip s
		Final state = nsStm conf
nsDeriv (Comp stm1 stm2) s = CompNS (conf :-->: state) devTree1 devTree2
	where
		conf = Inter (Comp stm1 stm2) s
		Final state = nsStm conf
		devTree1 = nsDeriv stm1 s
		devTree2 = nsDeriv stm2 s
nsDeriv (If bexp stm1 stm2) s 
	|bVal bexp s = IfTTNS (conf :-->: state) devTree
		where
			conf = Inter (If bexp stm1 stm2) s
			Final state = nsStm conf
			devTree = nsDeriv stm1 s
nsDeriv (If bexp stm1 stm2) s
	|otherwise = IfFFNS (conf :-->: state) devTree
		where
			conf = Inter (If bexp stm1 stm2) s
			Final state = nsStm conf
			devTree = nsDeriv stm2 s
	
nsDeriv (While bexp stm) s 
	|bVal bexp s = WhileTTNS (conf :-->: state) devTree1 devTree2
	where
		conf = Inter (While bexp stm) s
		Final state = nsStm conf
		devTree1 = nsDeriv stm s
		devTree2 = nsDeriv (While bexp stm) s
nsDeriv (While bexp stm) s 
	|otherwise = WhileFFNS (conf :-->: state) 
		where
			conf = Inter (While bexp stm) s
			Final state = nsStm conf