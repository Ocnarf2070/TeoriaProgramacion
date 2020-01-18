{-|

Programming Languages
Fall 2018

Implementation in Haskell of the concepts covered in Chapter 1 of
Nielson & Nielson, Semantics with Applications

Authors: Hasnaouia Meskini Iman, 
		González Sánchez Franco Emanuel 

-}

module Exercises01 where

import           Test.HUnit hiding (State)
import           While
import           Data.List

-- |----------------------------------------------------------------------
-- | Exercise 1
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Bin' for the binary numerals:

data Bit = O
         | I
         deriving (Show, Eq)

data Bin = MSB Bit
         | B Bin Bit
         deriving (Show, Eq)
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
binVal (MSB O)              = 0
binVal (MSB I)              = 1
binVal (B bin n)            = fp (binVal (MSB n)) (binVal bin)
  where
      fp sn sb = (+) sn (fs sb)
        where
          fs sb = 2 * sb

-- | Test your function with HUnit.

testBinVal :: Test
testBinVal = test ["value of zero"  ~: 0 ~=? binVal zero,
                   "value of one"   ~: 1 ~=? binVal one,
                   "value of three" ~: 3 ~=? binVal three,
                   "value of six"   ~: 6 ~=? binVal six]

-- | Define a function 'foldBin' to fold a value of type 'Bin'
foldBin :: (a -> Bit -> a)-> (Bit -> a)-> Bin -> a
foldBin b msb (MSB bit) = msb bit
foldBin b msb (B bin bit) = b (foldBin b msb bin) bit

-- | and use 'foldBin' to define a function 'binVal''  equivalent to 'binVal'.
binVal' :: Bin -> Integer
binVal' = foldBin b msb
  where
    msb O = 0
    msb I = 1
    b sol bit = (+) (msb bit) (2*sol)

-- | Test your function with HUnit.

testBinVal' :: Test
testBinVal' = test ["value of zero"  ~: 0 ~=? binVal' zero,
                   "value of one"    ~: 1 ~=? binVal' one,
                   "value of three"  ~: 3 ~=? binVal' three,
                   "value of six"    ~: 6 ~=? binVal' six]


-- | Define a function 'hammingWeight' that returns the number of ones occurring
-- | in a binary numeral.

hammingWeight :: Bin -> Integer
hammingWeight (MSB O)   = 0
hammingWeight (MSB I)   = 1
hammingWeight (B bin n) = (+) (hammingWeight (MSB n)) (hammingWeight bin)

-- | and use 'foldBin' to define a function 'hammingWeight''  equivalent to 'hammingWeight'.

hammingWeight' :: Bin -> Integer
hammingWeight' = foldBin b msb
  where
    msb O = 0
    msb I = 1
    b sol bit = (+) sol (msb bit)


-- | Test your functions with HUnit.

testHammingWeight :: Test
testHammingWeight = test ["value of zero"  ~: 0 ~=? hammingWeight zero,
                   "value of one"          ~: 1 ~=? hammingWeight one,
                   "value of three"        ~: 2 ~=? hammingWeight three,
                   "value of six"          ~: 2 ~=? hammingWeight six]

testHammingWeight' :: Test
testHammingWeight' = test ["value of zero"  ~: 0 ~=? hammingWeight' zero,
                   "value of one"           ~: 1 ~=? hammingWeight' one,
                   "value of three"         ~: 2 ~=? hammingWeight' three,
                   "value of six"           ~: 2 ~=? hammingWeight' six]

-- | Define a function 'complement' that returns the complement of a binary numeral

complement :: Bin -> Bin
complement (MSB O)   = (MSB I)
complement (MSB I)   = (MSB O)
complement (B bin bit) = f (complement bin) bit
    where
        f solR O = B solR I
        f solR I = B solR O


-- | and use 'foldBin' to define a function 'complement''  equivalent to 'complement'.

complement' :: Bin -> Bin
complement' = foldBin b msb
  where
        msb O = MSB I
        msb I = MSB O
        b sol O = B sol I
        b sol I = B sol O

-- | Test your functions with HUnit.

testComplement :: Test
testComplement = test ["complement of zero"  ~: one ~=? complement zero,
                   "complement of one"   ~: zero ~=? complement one,
                   "complement of three" ~: B (B (MSB I) O) O ~=? complement three,
                   "complement of six"   ~: B (B (MSB O) O) I ~=? complement six]

testComplement' :: Test
testComplement' = test ["complement of zero"  ~: one ~=? complement' zero,
                   "complement of one"   ~: zero ~=? complement' one,
                   "complement of three" ~: B (B (MSB I) O) O ~=? complement' three,
                   "complement of six"   ~: B (B (MSB O) O) I ~=? complement' six]


seventeen :: Bin
seventeen = B(B(B(B(MSB I)O)O)O)I

-- | Define a function 'normalize' that given a binary numeral trims leading zeroes.

normalize :: Bin -> Bin
normalize (MSB n) = MSB n
normalize (B bin bit) = f (normalize bin) bit
    where
      f (MSB O) bit = MSB bit
      f bin bit = B bin bit

-- | and use 'foldBin' to define a function 'normalize''  equivalent to 'normalize'.

normalize' :: Bin -> Bin
normalize' = foldBin b msb
  where
    msb bit = MSB bit
    b (MSB O) bit = MSB bit
    b bin bit = B bin bit

-- | Test your functions with HUnit.
testNormalize :: Test
testNormalize = test ["normalize zero"   ~: zero        ~=? normalize zero,
                   "normalize one"       ~: one         ~=? normalize one,
                   "normalize three"     ~: B (MSB I) I ~=? normalize three,
                   "normalize six"       ~: six         ~=? normalize six,
                   "normalize seventeen" ~: seventeen   ~=? normalize seventeen]
testNormalize' :: Test
testNormalize' = test ["normalize zero"                     ~: zero        ~=? normalize' zero,
                                      "normalize one"       ~: one         ~=? normalize' one,
                                      "normalize three"     ~: B (MSB I) I ~=? normalize' three,
                                      "normalize six"       ~: six         ~=? normalize' six,
                                      "normalize seventeen" ~: seventeen   ~=? normalize' seventeen]


-- |----------------------------------------------------------------------
-- | Exercise 2
-- |----------------------------------------------------------------------
-- | Define the function 'fvAexp' that computes the set of free variables
-- | occurring in an arithmetic expression. Ensure that each free variable
-- | occurs once in the resulting list.

fvAexp :: Aexp -> [Var]
fvAexp (V var)      = [var]
fvAexp (Add a1 a2)  = nub $ (++) (fvAexp a1) (fvAexp a2)
fvAexp (Mult a1 a2) = nub $ (++) (fvAexp a1) (fvAexp a2)
fvAexp (Sub a1 a2)  = nub $ (++) (fvAexp a1) (fvAexp a2)
fvAexp _            = []

-- | Test your function with HUnit.

n :: Aexp
n = (N 5)

x :: Var
x = "x"

var :: Aexp
var = (V x)

suma :: Aexp
suma = (Add (V x) n)

sub :: Aexp
sub = (Sub (V x) n)

mult :: Aexp
mult = (Mult (V x) n)

testFVA :: Test
testFVA  = test ["fvAexp num"  ~: []   ~=? fvAexp n,
                 "fvAexp var"  ~: [x]  ~=? fvAexp var,
                 "fvAexp suma"  ~: [x]  ~=? fvAexp suma,
                 "fvAexp sub"  ~: [x]  ~=? fvAexp sub,
                 "fvAexp mult" ~: [x]  ~=? fvAexp mult]

-- | Define the function 'fvBexp' that computes the set of free variables
-- | occurring in a Boolean expression.

fvBexp :: Bexp -> [Var]
fvBexp (Eq a1 a2)  = nub $ (++) (fvAexp a1) (fvAexp a2)
fvBexp (Le a1 a2)  = nub $ (++) (fvAexp a1) (fvAexp a2)
fvBexp (Neg b)     = fvBexp b
fvBexp (And b1 b2) = nub $ (++) (fvBexp b1) (fvBexp b2)
fvBexp _ = []

-- | Test your function with HUnit.
tt :: Bexp
tt = TRUE

ff :: Bexp
ff = FALSE

aexp1 = (Add (Mult (N 3) (V "x")) (Sub (N 7) (V "y")))

eq :: Bexp
eq = (Eq aexp1 aexp1)

le :: Bexp
le = (Le aexp1 aexp1)

neg :: Bexp
neg = (Neg eq)

andd :: Bexp
andd = (And eq le)

testFVB :: Test
testFVB  = test ["fvAexp TRUE"  ~: []     ~=? fvBexp tt,
                 "fvAexp var"   ~: []     ~=? fvBexp ff,
                 "fvAexp suma"  ~: [x,"y"]  ~=? fvBexp eq,
                 "fvAexp sub"   ~: [x,"y"]  ~=? fvBexp le,
                 "fvAexp mult"  ~: [x,"y"]  ~=? fvBexp neg,
                 "fvAexp mult"  ~: [x,"y"]  ~=? fvBexp andd]
-- |----------------------------------------------------------------------
-- | Exercise 3
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Subst' for representing substitutions:

data Subst = Var :->: Aexp

-- | define a function 'substAexp' that takes an arithmetic expression
-- | 'a' and a substitution 'y:->:a0' and returns the substitution a [y:->:a0];
-- | i.e., replaces every occurrence of 'y' in 'a' by 'a0'.



substAexp :: Aexp -> Subst -> Aexp
substAexp (V v1) (v2 :->: a0)
                                      | v1 == v2  = a0
                                      | otherwise = V v1
substAexp (Add a1 a2)  sus   = (Add (substAexp a1 sus) (substAexp a2 sus))
substAexp (Mult a1 a2) sus   = (Mult (substAexp a1 sus) (substAexp a2 sus))
substAexp (Sub a1 a2)  sus   = (Sub (substAexp a1 sus) (substAexp a2 sus))
substAexp num _ = num

-- | Test your function with HUnit.


testSubstAexp :: Test
testSubstAexp  = test ["subst Bexp num"   ~: n                           ~=? substAexp n subst,
                 "subst Bexp var"         ~: (V "SUSTITUIDA")            ~=? substAexp var subst,
                 "subst Bexp suma"        ~: (Add (V "SUSTITUIDA") n)    ~=? substAexp suma subst,
                 "subst Bexp sub"         ~: (Sub (V "SUSTITUIDA") n)    ~=? substAexp sub subst,
                 "subst Bexp mult"        ~: (Mult (V "SUSTITUIDA") n)   ~=? substAexp mult subst]

-- | Define a function 'substBexp' that implements substitution for
-- | Boolean expressions.

substBexp :: Bexp -> Subst -> Bexp
substBexp (Eq a1 a2) sus   = Eq (substAexp a1 sus) (substAexp a2 sus)
substBexp (Le a1 a2) sus   = Le(substAexp a1 sus) (substAexp a2 sus)
substBexp (Neg b) sus      = Neg $ substBexp b sus
substBexp (And b1 b2)  sus = And (substBexp b1 sus) (substBexp b2 sus)
substBexp b _              = b

-- | Test your function with HUnit.
subst :: Subst
subst = ("x" :->: (V "SUSTITUIDA"))

sust= (Add (Mult (N 3) (V "SUSTITUIDA")) (Sub (N 7) (V "y")))

testSubstBexp :: Test
testSubstBexp  = test ["substBexp TRUE"  ~: tt                                    ~=? substBexp tt subst,
                 "substBexp FALSE"       ~: ff                                    ~=? substBexp ff subst,
                 "substBexp EQ"          ~: (Eq sust sust)                        ~=? substBexp eq subst,
                 "substBexp LE"          ~: (Le sust sust)                        ~=? substBexp le subst,
                 "substBexp NEG"         ~: (Neg (Eq sust sust))                  ~=? substBexp neg subst,
                 "substBexp AND"         ~: (And (Eq sust sust) (Le sust sust) )  ~=? substBexp andd subst]

-- |----------------------------------------------------------------------
-- | Exercise 4
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Update' for state updates:

data Update = Var :=>: Z

-- | define a function 'update' that takes a state 's' and an update 'x :=> v'
-- | and returns the updated state 's [x :=> v]'

update :: State -> Update -> State
update s (y :=>: v) = (\x -> if x == y then v else s x)


-- | Test your function with HUnit.
up1 :: Update
up1 = "x" :=>: 1

up2 :: Update
up2 = "y" :=>: 1

testUpdate :: Test
testUpdate = test ["update (x -> 3) (x :=> 1)"   ~: 1  ~=? (update sInit up1) "x",
                  "update (x -> 3) (y :=> 1)"    ~: 3  ~=? (update sInit up2) "x"]



-- | Define a function 'updates' that takes a state 's' and a list of updates
-- | 'us' and returns the updated states resulting from applying the updates
-- | in 'us' from head to tail. For example:
-- |
-- |    updates s ["x" :=>: 1, "y" :=>: 2, "x" :=>: 3]
-- |
-- | returns a state that binds "x" to 3 (the most recent update for "x").

updates :: State ->  [Update] -> State
updates s []     = s
updates s (x:xs) = updates (update s x) xs

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

-- | Test your function with HUnit.
testAVal' :: Test
testAVal'  = test ["aVal' (x->3) num"       ~: 5    ~=? aVal' n sInit ,
                 "aVal' var"         ~: 3    ~=? aVal' var sInit,
                 "aVal' suma"        ~: 8    ~=? aVal' suma sInit,
                 "aVal' sub"         ~: -2    ~=? aVal' sub  sInit,
                 "aVal' mult"        ~: 15   ~=? aVal' mult  sInit]

testfvAexp' :: Test
testfvAexp'  = test ["fvAexp' num"  ~: []   ~=? fvAexp' n,
                                  "fvAexp' var"  ~: [x]  ~=? fvAexp' var,
                                  "fvAexp' suma"  ~: [x]  ~=? fvAexp' suma,
                                  "fvAexp' sub"  ~: [x]  ~=? fvAexp' sub,
                                  "fvAexp' mult" ~: [x]  ~=? fvAexp' mult]

testSubstAexp' :: Test
testSubstAexp'  = test ["substAexp' num"   ~: n                           ~=? substAexp' n subst,
                                  "substAexp' var"         ~: (V "SUSTITUIDA")            ~=? substAexp' var subst,
                                  "substAexp' suma"        ~: (Add (V "SUSTITUIDA") n)    ~=? substAexp' suma subst,
                                  "substAexp' sub"         ~: (Sub (V "SUSTITUIDA") n)    ~=? substAexp' sub subst,
                                  "substAexp' mult"        ~: (Mult (V "SUSTITUIDA") n)   ~=? substAexp' mult subst]

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
      f a        = aVal' a s
      feq a1 a2  = (==) a1 a2
      fle a1 a2  = (<=) a1 a2
      fneg b     = not b
      fand b1 b2 = (&&) b1 b2


fvBexp' :: Bexp -> [Var]
fvBexp' = foldBexp [] [] feq fle fneg fand f
    where
      f a        = fvAexp' a
      feq a1 a2  = nub $ (++) a1 a2
      fle a1 a2  = nub $ (++) a1 a2
      fneg b     = b
      fand b1 b2 = nub $ (++) b1 b2


substBexp' :: Bexp -> Subst -> Bexp
substBexp' bexp sus = foldBexp (TRUE) (FALSE) feq fle fneg fand f bexp
    where
      f a        = substAexp' a sus
      feq a1 a2  = Eq a1 a2
      fle a1 a2  = Le a1 a2
      fneg b     = Neg b
      fand b1 b2 = And b1 b2

-- | Test your function with HUnit.

testBVal' :: Test
testBVal' = test ["bVal' ' TRUE"  ~: True   ~=? bVal' tt sInit,
                 "bVal'  var"     ~: False  ~=? bVal'  ff sInit,
                 "bVal'  suma"    ~: True   ~=? bVal' eq sInit,
                 "bVal'  sub"     ~: True  ~=? bVal' le sInit,
                 "bVal'  mult"    ~: False  ~=? bVal' neg sInit,
                 "bVal'  mult"    ~: True  ~=? bVal' andd sInit]

testFvBexp' :: Test
testFvBexp'  = test ["fvBexp' TRUE"  ~: []     ~=? fvBexp' tt,
                 "fvBexp' var"   ~: []     ~=? fvBexp' ff,
                 "fvBexp' suma"  ~: [x,"y"]  ~=? fvBexp' eq,
                 "fvBexp' sub"   ~: [x,"y"]  ~=? fvBexp' le,
                 "fvBexp' mult"  ~: [x,"y"]  ~=? fvBexp' neg,
                 "fvBexp' mult"  ~: [x,"y"]  ~=? fvBexp' andd]

testSubstBexp' :: Test
testSubstBexp'  = test ["substBexp' TRUE" ~: tt                                   ~=? substBexp' tt subst,
                 "substBexp' FALSE"       ~: ff                                    ~=? substBexp' ff subst,
                 "substBexp' EQ"          ~: (Eq sust sust)                        ~=? substBexp' eq subst,
                 "substBexp' LE"          ~: (Le sust sust)                        ~=? substBexp' le subst,
                 "substBexp' NEG"         ~: (Neg (Eq sust sust))                  ~=? substBexp' neg subst,
                 "substBexp' AND"         ~: (And (Eq sust sust) (Le sust sust) )  ~=? substBexp' andd subst]
