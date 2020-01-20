module Test.Operation.Annihilative where

import Test.Logic


-- | \( \forall a: (u \# a) \equiv u \)
--
-- Right annihilativity of an element /u/ with respect to an operator /#/.
--
-- For example, @False@ is a right annihilative element of @||@.
--
annihilative :: Eq r => (r -> r -> r) -> r -> (r -> Bool)
annihilative = annihilative_on (==)

-- | \( \forall a: (a \# u) \equiv u \)
--
-- Left annihilativity of an element /u/ with respect to an operator /#/.
--
-- For example, @Nothing@ is a right annihilative element of @*>@.
--
annihilative' :: Eq r => (r -> r -> r) -> r -> (r -> Bool)
annihilative' = annihilative_on' (==)

annihilative_on :: Rel r b -> (r -> r -> r) -> r -> (r -> b)
annihilative_on (~~) (#) u a = (u # a) ~~ u

annihilative_on' :: Rel r b -> (r -> r -> r) -> r -> (r -> b)
annihilative_on' (~~) (#) u a = (a # u) ~~ u
