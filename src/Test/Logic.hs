{-# LANGUAGE TypeOperators              #-}
module Test.Logic where

import qualified Control.Monad as M (join)
import Data.Tuple (swap)
import Data.Void

type Rel r b = r -> r -> b

type (+) = Either

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

xor3 :: Bool -> Bool -> Bool -> Bool
xor3 a b c = (a `xor` (b `xor` c)) && not (a && b && c)

infixr 0 ==>

(==>) :: Bool -> Bool -> Bool
(==>) a b = not a || b

iff :: Bool -> Bool -> Bool
iff a b = a ==> b && b ==> a

infixr 1 <==>

(<==>) :: Bool -> Bool -> Bool
(<==>) = iff

rgt :: (a -> b) -> a + b -> b
rgt f = either f id
{-# INLINE rgt #-}

rgt' :: Void + b -> b
rgt' = rgt absurd
{-# INLINE rgt' #-}

lft :: (b -> a) -> a + b -> a
lft f = either id f
{-# INLINE lft #-}

lft' :: a + Void -> a
lft' = lft absurd
{-# INLINE lft' #-}

eswap :: (a1 + a2) -> (a2 + a1)
eswap (Left x) = Right x
eswap (Right x) = Left x
{-# INLINE eswap #-}

fork :: a -> (a , a)
fork = M.join (,)
{-# INLINE fork #-}

join :: (a + a) -> a
join = M.join either id
{-# INLINE join #-}

eval :: (a , a -> b) -> b
eval = uncurry $ flip id
{-# INLINE eval #-}

apply :: (b -> a , b) -> a
apply = uncurry id
{-# INLINE apply #-}

