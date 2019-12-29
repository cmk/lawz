module Test.Operation.Neutral where

import Test.Util

-- | \( \forall a: (u \# a) \equiv a \)
--
-- Right neutrality of a unit /u/ with respect to an operator /#/.
--
-- For example, an implementation of 'Monoid' must satisfy @neutral (<>) mempty@
--
neutral :: Eq r => (r -> r -> r) -> r -> (r -> Bool)
neutral = neutral_on (==)

-- | \( \forall a: (a \# u) \equiv a \)
--
-- Left neutrality of a unit /u/ with respect to an operator /#/.
--
-- For example, an implementation of 'Monoid' must satisfy @neutral (<>) mempty@
--
neutral' :: Eq r => (r -> r -> r) -> r -> (r -> Bool)
neutral' = neutral_on' (==)

neutral_on :: Rel r -> (r -> r -> r) -> r -> (r -> Bool)
neutral_on (~~) (#) u a = (u # a) ~~ a

neutral_on' :: Rel r -> (r -> r -> r) -> r -> (r -> Bool)
neutral_on' (~~) (#) u a = (a # u) ~~ a
