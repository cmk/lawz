module Test.Property.Operation.Commutative where

import Test.Property.Util

-- | \( \forall a, b: a \# b \equiv b \# a \)
--
commutative :: Eq r => (r -> r -> r) -> r -> r -> Bool
commutative = commutative_on (==)

-- | \( \forall a, b: a \# b \doteq b \# a \)
--
commutative_on :: Rel r -> (r -> r -> r) -> r -> r -> Bool
commutative_on (~~) (#) a b = (a # b) ~~ (b # a)

