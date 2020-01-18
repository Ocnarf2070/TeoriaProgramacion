{-|

Programming Languages
Fall 2018

Implementation in Haskell of the concepts covered in Chapter 1 of
Nielson & Nielson, Semantics with Applications

Author: Franco Emanuel González Sánchez

-}

module Exercises01 where

import           Test.HUnit hiding (State)
import           While
import			 Data.List

-- |----------------------------------------------------------------------
-- | Exercise 1
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Bin' for the binary numerals:

data Bit = O
         | I
         deriving (Show,Eq)

data Bin = MSB Bit
         | B Bin Bit
		 deriving (Show,Eq)

-- | and the following values of type 'Bin':

zero :: Bin
zero = MSB O

one :: Bin
one = MSB I

three :: Bin
three = B (B (MSB O) I) I

six :: Bin
six = B (B (MSB I) I) O

-- | define a semantic function 'binVal' that associates
-- | a number (in the decimal system) to each binary numeral.

binVal :: Bin -> Z
binVal (MSB O) = 0
binVal (MSB I) = 1
--binVal (B bin O) = 2 * binVal bin
--binVal (B bin I) = 2 * binVal bin + 1
binVal (B bin a) = f (binVal bin) a
	where 
		f b O = 2*b
		f b I = 2*b + 1


-- | Test your function with HUnit.

testBinVal :: Test
testBinVal = test ["value of zero"  ~: 0 ~=? binVal zero,
                   "value of one"   ~: 1 ~=? binVal one,
                   "value of three" ~: 3 ~=? binVal three,
                   "value of six"   ~: 6 ~=? binVal six]

-- | Define a function 'foldBin' to fold a value of type 'Bin'

foldBin :: (b -> Bit -> b) -> (Bit -> b) -> Bin -> b
foldBin fr fb (MSB a) = fb a
foldBin fr fb (B bin a) = fr (foldBin fr fb bin) a


-- | and use 'foldBin' to define a function 'binVal''  equivalent to 'binVal'.

binVal' :: Bin -> Integer
binVal' = foldBin fr fb 
	where
		fb O = 0
		fb I = 1
		fr b a = 2*b + fb a


-- | Test your function with HUnit.

testBinVal' :: Test
testBinVal' = test ["value of zero"  ~: 0 ~=? binVal' zero,
                   "value of one"   ~: 1 ~=? binVal' one,
                   "value of three" ~: 3 ~=? binVal' three,
                   "value of six"   ~: 6 ~=? binVal' six]

-- | Define a function 'hammingWeight' that returns the number of ones occurring
-- | in a binary numeral.

hammingWeight :: Bin -> Integer
hammingWeight (MSB O) = 0
hammingWeight (MSB I) = 1
--hammingWeight (B bin O) = hammingWeight bin
--hammingWeight (B bin I) = hammingWeight bin + 1
hammingWeight (B bin a) = f (hammingWeight bin) a
	where 
		f b O = b
		f b I = b + 1

-- | and use 'foldBin' to define a function 'hammingWeight''  equivalent to 'hammingWeight'.

hammingWeight' :: Bin -> Integer
hammingWeight' = foldBin fr fb 
	where
		fb O = 0
		fb I = 1 
		fr b a = b + fb a

-- | Test your functions with HUnit.

testHammingWeight :: Test
testHammingWeight = test ["hamming of zero"  ~: 0 ~=? hammingWeight zero,
                   "hamming of one"   ~: 1 ~=? hammingWeight one,
                   "hamming of three" ~: 2 ~=? hammingWeight three,
                   "hamming of six"   ~: 2 ~=? hammingWeight six]

testHammingWeight' :: Test
testHammingWeight' = test ["hamming of zero"  ~: 0 ~=? hammingWeight' zero,
                   "hamming of one"   ~: 1 ~=? hammingWeight' one,
                   "hamming of three" ~: 2 ~=? hammingWeight' three,
                   "hamming of six"   ~: 2 ~=? hammingWeight' six]



-- | Define a function 'complement' that returns the complement of a binary numeral

complement :: Bin -> Bin
complement (MSB O) = MSB I
complement (MSB I) = MSB O
--complement (B bin O) = B (complement bin) I
--complement (B bin I) = B (complement bin) O
complement (B bin a) = f (complement bin) a
	where 
		f b O = B b I
		f b I = B b O

-- | and use 'foldBin' to define a function 'complement''  equivalent to 'complement'.

complement' :: Bin -> Bin
complement' = foldBin fr fb 
	where
		fb O = MSB I
		fb I = MSB O
		fr b O = B b I
		fr b I = B b O

-- | Test your functions with HUnit.

testComplement :: Test
testComplement = test ["complement of zero"  ~: (MSB I) ~=? complement zero,
                   "complement of one"   ~: (MSB O) ~=? complement one,
                   "complement of three" ~: (B (B (MSB I) O) O) ~=? complement three,
                   "complement of six"   ~: (B (B (MSB O) O) I)  ~=? complement six]

testComplement' :: Test
testComplement' = test ["complement of zero"  ~: (MSB I) ~=? complement' zero,
                   "complement of one"   ~: (MSB O) ~=? complement' one,
                   "complement of three" ~: (B (B (MSB I) O) O) ~=? complement' three,
                   "complement of six"   ~: (B (B (MSB O) O) I) ~=? complement' six]


three' :: Bin
three' = B (B (B (MSB O) O) I) I

five :: Bin
five = B (B (B (B (B (B (B (MSB O) O) O) O) O) I) O) I
				   
-- | Define a function 'normalize' that given a binary numeral trims leading zeroes.

normalize :: Bin -> Bin
normalize (MSB a) = MSB a
normalize (B bin a) = f (normalize bin) a
	where 
		f (MSB O) a = MSB a
		f bin a = B bin a

-- | and use 'foldBin' to define a function 'normalize''  equivalent to 'normalize'.

normalize' :: Bin -> Bin
normalize' = foldBin fr fb 
	where
		fb a = MSB a
		fr (MSB O) a = MSB a
		fr bin a = B bin a

-- | Test your functions with HUnit.

testNormalize :: Test
testNormalize = test ["normalize of zero"  ~: (MSB O) ~=? normalize zero,
                   "normalize of one"   ~: (MSB I) ~=? normalize one,
                   "normalize of three" ~: (B (MSB I) I) ~=? normalize three,
                   "normalize of six"   ~: (B (B (MSB I) I) O)  ~=? normalize six]

testNormalize' :: Test
testNormalize' = test ["normalize of zero"  ~: (MSB O) ~=? normalize' zero,
                   "normalize of one"   ~: (MSB I) ~=? normalize' one,
                   "normalize of three" ~: (B (MSB I) I) ~=? normalize' three,
                   "normalize of six"   ~: (B (B (MSB I) I) O) ~=? normalize' six]

-- |----------------------------------------------------------------------
-- | Exercise 2
-- |----------------------------------------------------------------------
-- | Define the function 'fvAexp' that computes the set of free variables
-- | occurring in an arithmetic expression. Ensure that each free variable
-- | occurs once in the resulting list.

expA1 = Add (Mult (N 3) (V "x")) (Sub (N 7) (V "y")) -- (3 * x) + (7 - y)
expA2 = Mult (V "x") (V "x") -- x * x
expA3 = Sub (Mult (Sub (V "x") (V "y")) (V "z")) expA2 -- ((x - y) * z) - (x * x)
expB1 = Eq (V "x") (N 3) -- x == 3
expB2 = Le (Add (V "y") (N 2)) expA2 -- (y + 2) <= (x * x)
expB3 = Neg expB1 -- ¬ (x == 3)
expB4 = And	expB1 (Neg expB3) -- (x == 3) && ( ¬ ( ¬ (x == 3)))
expB5 = And expB1 (Neg expB2) -- (x == 3) && ( ¬ ((y + 2) <= (x * x)))

fvAexp :: Aexp -> [Var]
fvAexp (V var) = [var]
fvAexp (Add exp1 exp2) = nub $ (++) (fvAexp exp1) (fvAexp exp2)
fvAexp (Mult exp1 exp2) = nub $ (++) (fvAexp exp1) (fvAexp exp2)
fvAexp (Sub exp1 exp2) = nub $ (++) (fvAexp exp1) (fvAexp exp2)
fvAexp _ = []

-- | Test your function with HUnit.

-- todo

-- | Define the function 'fvBexp' that computes the set of free variables
-- | occurring in a Boolean expression.

fvBexp :: Bexp -> [Var]
fvBexp (Eq aexp1 aexp2) = nub $ (++) (fvAexp aexp1) (fvAexp aexp2)
fvBexp (Le aexp1 aexp2) = nub $ (++) (fvAexp aexp1) (fvAexp aexp2)
fvBexp (Neg bexp) = fvBexp bexp
fvBexp (And bexp1 bexp2) = nub $ (++) (fvBexp bexp1) (fvBexp bexp2)
fvBexp _ = []

-- | Test your function with HUnit.

-- todo

-- |----------------------------------------------------------------------
-- | Exercise 3
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Subst' for representing substitutions:

data Subst = Var :->: Aexp

-- | define a function 'substAexp' that takes an arithmetic expression
-- | 'a' and a substitution 'y:->:a0' and returns the substitution a [y:->:a0];
-- | i.e., replaces every occurrence of 'y' in 'a' by 'a0'.

substAexp :: Aexp -> Subst -> Aexp
substAexp (V var) ( svar :->: exp) 
	| svar == var = exp
	| otherwise = V var
substAexp (Add exp1 exp2) subst = Add (substAexp exp1 subst) (substAexp exp2 subst)
substAexp (Mult exp1 exp2) subst = Mult (substAexp exp1 subst) (substAexp exp2 subst)
substAexp (Sub exp1 exp2) subst = Sub (substAexp exp1 subst) (substAexp exp2 subst)
substAexp num subst = num

-- | Test your function with HUnit.

-- todo

-- | Define a function 'substBexp' that implements substitution for
-- | Boolean expressions.

substBexp :: Bexp -> Subst -> Bexp
substBexp (Eq aexp1 aexp2) subst = Eq (substAexp aexp1 subst) (substAexp aexp2 subst)
substBexp (Le aexp1 aexp2) subst = Le (substAexp aexp1 subst) (substAexp aexp2 subst)
substBexp (Neg bexp) subst = Neg $ substBexp bexp subst
substBexp (And bexp1 bexp2) subst = And (substBexp bexp1 subst) (substBexp bexp2 subst)
substBexp bool subst = bool

-- | Test your function with HUnit.

-- todo

-- |----------------------------------------------------------------------
-- | Exercise 4
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Update' for state updates:

data Update = Var :=>: Z

-- | define a function 'update' that takes a state 's' and an update 'x :=> v'
-- | and returns the updated state 's [x :=> v]'

update :: State -> Update -> State
update s (y :=>: n) = (\x -> if x == y then n else s x)

-- | Test your function with HUnit.

-- todo

-- | Define a function 'updates' that takes a state 's' and a list of updates
-- | 'us' and returns the updated states resulting from applying the updates
-- | in 'us' from head to tail. For example:
-- |
-- |    updates s ["x" :=>: 1, "y" :=>: 2, "x" :=>: 3]
-- |
-- | returns a state that binds "x" to 3 (the most recent update for "x").

updates :: State ->  [Update] -> State
updates s [x] = update s x
updates s xs = update (updates s ls) l
	where 
		l = last xs
		ls = init xs

-- |----------------------------------------------------------------------
-- | Exercise 5
-- |----------------------------------------------------------------------
-- | Define a function 'foldAexp' to fold an arithmetic expression

foldAexp :: (a->a->a) -> (a->a->a) -> (a->a->a) -> (Var->a) -> (Integer->a) -> Aexp -> a
foldAexp fa fm fs v n (N num)  = n num
foldAexp fa fm fs v n (V var)  = v var
foldAexp fa fm fs v n (Add exp1 exp2)   = fa (foldAexp fa fm fs v n exp1) (foldAexp fa fm fs v n exp2)
foldAexp fa fm fs v n (Mult exp1 exp2)  = fm (foldAexp fa fm fs v n exp1) (foldAexp fa fm fs v n exp2)
foldAexp fa fm fs v n (Sub exp1 exp2)   = fs (foldAexp fa fm fs v n exp1) (foldAexp fa fm fs v n exp2)


-- | Use 'foldAexp' to define the functions 'aVal'', 'fvAexp'', and 'substAexp''
-- | and test your definitions with HUnit.

aVal' :: Aexp -> State -> Z
aVal' exp s          =  foldAexp fa fm fs v n exp
    where
      n  num    = num
      v  var    = s var
      fa a1 a2  = (+) (a1) (a2)
      fm a1 a2  = (*) (a1) (a2)
      fs a1 a2  = (-) (a1) (a2)

fvAexp' :: Aexp -> [Var]
fvAexp' = foldAexp fa fm fs v n
    where
      n num = []
      v var = [var]
      fa a1 a2 = nub $ (++) a1 a2
      fm a1 a2 = nub $ (++) a1 a2
      fs a1 a2 = nub $ (++) a1 a2

substAexp' :: Aexp -> Subst -> Aexp
substAexp' exp (svar :->: a0) = foldAexp fa fm fs v n exp
  where
    n num = N num
    v var
        | svar == var = a0
        | otherwise = V var
    fa a1 a2 = Add (a1) (a2)
    fm a1 a2 = Mult (a1) (a2)
    fs a1 a2 = Sub (a1) (a2)

-- | Define a function 'foldBexp' to fold a Boolean expression and use it
-- | to define the functions 'bVal'', 'fvBexp'', and 'substAexp''. Test
-- | your definitions with HUnit.

foldBexp :: b -> b -> (a->a->b) -> (a->a->b) -> (b->b) -> (b->b->b) -> (Aexp -> a) -> Bexp -> b
foldBexp fbt fbf feq fle fneg fand f TRUE              = fbt
foldBexp fbt fbf feq fle fneg fand f FALSE             = fbf
foldBexp fbt fbf feq fle fneg fand f (Eq aexp1 aexp2)  = feq (f aexp1) (f aexp2)
foldBexp fbt fbf feq fle fneg fand f (Le aexp1 aexp2)  = fle (f aexp1) (f aexp2)
foldBexp fbt fbf feq fle fneg fand f (Neg bexp)        = fneg $ foldBexp fbt fbf feq fle fneg fand f bexp
foldBexp fbt fbf feq fle fneg fand f (And bexp1 bexp2) = fand (foldBexp fbt fbf feq fle fneg fand f bexp1) (foldBexp fbt fbf feq fle fneg fand f bexp2)


bVal' :: Bexp -> State -> Bool
bVal' bexp s = foldBexp (True) (False) feq fle fneg fand f bexp
    where
      f a = aVal' a s
      feq a1 a2 = (==) a1 a2
      fle a1 a2 = (<=) a1 a2
      fneg b = not b
      fand b1 b2 = (&&) b1 b2
      

fvBexp' :: Bexp -> [Var]
fvBexp' = foldBexp fb fb feq fle fneg fand f
	where
		fb = []
		f = fvAexp'
		feq a1 a2 = nub $ a1 ++ a2
		fle a1 a2 = nub $ a1 ++ a2
		fneg b = b
		fand b1 b2 = nub $ b1 ++ b2

substBexp' :: Bexp -> Subst -> Bexp
substBexp' bexp s = foldBexp TRUE FALSE feq fle fneg fand f bexp
	where
		f a = substAexp' a s
		feq a1 a2 = Eq a1 a2
		fle a1 a2 = Le a1 a2
		fneg b = Neg b
		fand b1 b2 = And b1 b2

