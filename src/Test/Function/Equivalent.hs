module Test.Function.Equivalent where

import Test.Util



-- | \( \forall a: f a \equiv g a \)
--
equivalent :: Eq r => (r -> r) -> (r -> r) -> (r -> Bool)
equivalent = equivalent_on (==)


-- | \( \forall a: f a \doteq g a \)
--
equivalent_on :: Rel r -> (r -> r) -> (r -> r) -> (r -> Bool)
equivalent_on (~~) f g a = f a ~~ g a
