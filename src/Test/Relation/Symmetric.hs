-- | See <https://en.wikipedia.org/wiki/Binary_relation#Properties>.
--
-- Note that these properties do not exhaust all of the possibilities.
--
-- As an example over the natural numbers, the relation \(a \# b \) defined by 
-- \( a > 2 \) is neither symmetric nor antisymmetric, let alone asymmetric.
module Test.Relation.Symmetric where

import Test.Util


-- | \( \forall a, b: (a \# b) \Leftrightarrow (b \# a) \)
--
-- For example, "is a blood relative of" is a symmetric relation, because 
-- A is a blood relative of B if and only if B is a blood relative of A.
--
symmetric :: (r -> r -> Bool) -> r -> r -> Bool
symmetric (#) a b = (a # b) <==> (b # a)


-- | \( \forall a, b: (a \# b) \Rightarrow \neg (b \# a) \)
--
-- For example, > is an asymmetric relation, but ≥ is not.
--
-- A relation is asymmetric if and only if it is both antisymmetric and irreflexive.
-- 
asymmetric :: (r -> r -> Bool) -> r -> r -> Bool
asymmetric (#) a b = (a # b) ==> not (b # a)


-- | \( \forall a, b: (a \# b) \wedge (b \# a) \Rightarrow a \equiv b \)
--
-- For example, ≥ is an antisymmetric relation; so is >, but vacuously 
-- (the condition in the definition is always false).
-- 
antisymmetric :: Eq r => (r -> r -> Bool) -> r -> r -> Bool
antisymmetric = antisymmetric_on (==)


-- | \( \forall a, b: (a \# b) \wedge (b \# a) \Rightarrow a \doteq b \)
-- 
antisymmetric_on :: (r -> r -> Bool) -> (r -> r -> Bool) -> r -> r -> Bool
antisymmetric_on (~~) (#) a b = (a # b) && (b # a) ==> (a ~~ b)
