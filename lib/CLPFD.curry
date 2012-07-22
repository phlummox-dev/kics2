------------------------------------------------------------------------------
--- Library for finite domain constraint solving.
--- <p>
--- The general structure of a specification of an FD problem is as follows:
--- 
--- <code>domain_constraint & fd_constraint & labeling</code>
--- 
--- where:
--- 
--- <code>domain_constraint</code>
--- specifies the possible range of the FD variables (see constraint <code>domain</code>)
--- 
--- <code>fd_constraint</code>
--- specifies the constraint to be satisfied by a valid solution
--- (see constraints #+, #-, allDifferent, etc below)
--- 
--- <code>labeling</code>
--- is a labeling function to search for a concrete solution.
---

------------------------------------------------------------------------------

module CLPFD(domain,(+#),(-#),(*#),(=#), (/=#), (<#), (<=#), (>#), (>=#),allDifferent, sum, labeling, labelingWith, LabelingStrategy (..), genVars) where

-- The operator declarations are similar to the standard arithmetic
-- and relational operators.

infixl 7 *#
infixl 6 +#, -#
infix  4 =#, /=#, <#, <=#, >#, >=#

-- Constraint to specify the domain of all finite domain variables
-- @param vs - list of finite domain variables
-- @param l - lower boundary for all variables in vs
-- @param u - upper boundary for all variables in vs
domain :: [Int] -> Int -> Int -> Success
domain vs l u = (((prim_domain vs) $!! l) $!! u) listVar where listVar free

prim_domain :: [Int] -> Int -> Int -> [Int] -> Success
prim_domain external

-- Addition of FD variables.
(+#)   :: Int -> Int -> Int
x +# y = ((prim_FD_plus $!! x) $!! y) result where result free

prim_FD_plus :: Int -> Int -> Int -> Int
prim_FD_plus external

-- Subtraction of FD variables.
(-#)   :: Int -> Int -> Int
x -# y = ((prim_FD_minus $!! x) $!! y) result where result free

prim_FD_minus :: Int -> Int -> Int -> Int
prim_FD_minus external

-- Multiplication of FD variables.
(*#)   :: Int -> Int -> Int
x *# y = ((prim_FD_times $!! x) $!! y) result where result free

prim_FD_times :: Int -> Int -> Int -> Int
prim_FD_times external


-- Equality of FD variables.
(=#)   :: Int -> Int -> Success
x =# y = (prim_FD_equal $!! x) $!! y

prim_FD_equal :: Int -> Int -> Success
prim_FD_equal external


-- Disequality of FD variables.
(/=#)  :: Int -> Int -> Success
x /=# y = (prim_FD_notequal $!! x) $!! y

prim_FD_notequal :: Int -> Int -> Success
prim_FD_notequal external

-- "Less than" constraint on FD variables.
(<#)   :: Int -> Int -> Success
x <# y = (prim_FD_le $!! x) $!! y

prim_FD_le :: Int -> Int -> Success
prim_FD_le external

-- "Less than or equal" constraint on FD variables.
(<=#)  :: Int -> Int -> Success
x <=# y = (prim_FD_leq $!! x) $!! y

prim_FD_leq :: Int -> Int -> Success
prim_FD_leq external

-- "Greater than" constraint on FD variables.
(>#)   :: Int -> Int -> Success
x ># y = (prim_FD_ge $!! x) $!! y

prim_FD_ge :: Int -> Int -> Success
prim_FD_ge external

-- "Greater than or equal" constraint on FD variables.
(>=#)  :: Int -> Int -> Success
x >=# y = (prim_FD_geq $!! x) $!! y

prim_FD_geq :: Int -> Int -> Success
prim_FD_geq external

-- "All different" constraint on FD variables.
allDifferent :: [Int] -> Success
allDifferent vs = (prim_allDifferent vs) listVar where listVar free

prim_allDifferent :: [Int] -> [Int] -> Success
prim_allDifferent external

-- "Sum" constraint on FD variables.
-- @return sum of given variables
sum :: [Int] -> Int
sum vs = prim_sum vs result listVar where result,listVar free

prim_sum :: [Int] -> Int -> [Int] -> Int
prim_sum external

-- label FD variables in order
-- @param var - helper variable for construction of solutions
labeling :: [Int] -> Success
labeling vs = (prim_labelingWith InOrder) vs listVar labelVar where listVar,labelVar free

-- label FD variables with strategy
-- @param var - helper variable for construction of solutions
labelingWith :: LabelingStrategy -> [Int] -> Success
labelingWith strategy vs = (prim_labelingWith $## strategy) vs listVar labelVar where listVar,labelVar free

prim_labelingWith :: LabelingStrategy -> [Int] -> [Int] -> [Int] -> Success
prim_labelingWith external

-- supported labeling strategies
data LabelingStrategy = InOrder
                      | FirstFail
                      | MiddleOut
                      | EndsOut

-- generate list of n FD variables
genVars :: Int -> [Int]
genVars n = if n==0 then [] else var : genVars (n-1)  where var free


-- end of library CLPFD
