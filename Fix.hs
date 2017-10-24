{-# LANGUAGE DeriveFunctor,
             TypeSynonymInstances,
             FlexibleInstances,
             ExistentialQuantification,
             Rank2Types,
             AllowAmbiguousTypes
#-}
module Fix where

import Prelude hiding (sum ,length, succ, either, head, last)

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)
either (f, g) (Left x) = f x
either (f, g) (Right x) = g x


newtype Fix f = In { out :: f (Fix f) }

-- catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out
-- anamorphism
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = In . fmap (ana psi) . psi
-- hylomorphism
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo phi psi = phi . fmap (hylo phi psi) . psi
-- metamorphism
meta :: Functor f => (f a -> a) -> (a -> f a) -> Fix f -> Fix f
meta phi psi = ana psi . cata phi
-- paramorphism
para :: Functor f => (f (Fix f, t) -> t) -> Fix f -> t
para phi = phi . fmap (pair (id, para phi)) . out
-- apomorphism
apo :: Functor f => (t -> f (Either (Fix f) t)) -> t -> Fix f
apo psi = In . fmap (either (id, apo psi)) . psi
-- histomorphism
newtype HisF f a = His { unHis :: (a, f (HisF f a)) }
type His a = forall f. HisF f a
head :: HisF f t -> t
head = fst . unHis
histo :: Functor f => (f (HisF f t) -> t) -> Fix f -> t
histo phi = head . cata (His . pair (phi, id))
-- futumorphism
newtype FutF f a = Fut { unFut :: Either a (f (FutF f a)) }
type Fut a =  forall f. FutF f a
last :: a -> FutF f a
last = Fut . Left
futu :: Functor f => (t -> f (FutF f t)) -> t -> Fix f
futu psi = ana (either (psi, id) . unFut) . last
-- chronomorphism
chrono :: Functor f => (f (HisF f b) -> b) -> (a -> f (FutF f a)) -> a -> b
chrono phi psi = histo phi . futu psi
-- zygomorphism
zygo :: Functor f => (f a -> a) -> (f (a, b) -> b) -> Fix f -> b
zygo f phi = snd . cata (pair (f . fmap fst, phi))
-- cozygomorphism
cozygo :: Functor f => (a -> f a) -> (b -> f (Either a b)) -> b -> Fix f
cozygo f psi = ana (either (fmap Left . f, psi)) . Right
-- dynamorphism
dyna :: Functor f => (f (HisF f b) -> b) -> (a -> f a) -> a -> b
dyna f g = chrono f (fmap last . g)
-- codynamorphism
codyna :: Functor f => (f b -> b) -> (a -> f (FutF f a)) -> a -> b
codyna f g = chrono (f . fmap head) g
-- mutumorphism
mutu :: Functor f => (a -> b) -> (f a -> a) -> Fix f -> b
mutu proj phi = proj . cata phi

-- | Natural Number
data NatF x = Zero | Succ x deriving (Show, Functor)

type Nat = Fix NatF

zero :: Nat
zero = In Zero
succ :: Nat -> Nat
succ n = In (Succ n)

instance Show Nat where
  show n = "(" ++ show (out n) ++ ")"

toInt :: Nat -> Int
toInt = cata phi
  where
    phi Zero = 0
    phi (Succ n) = 1 + n -- !?

double :: Nat -> Nat
double = cata phi
  where
    phi Zero = zero
    phi (Succ n) = succ (succ n) -- !?

fromInt :: Int -> Nat
fromInt = ana psi
  where
    psi n = if n <= 0 then Zero else Succ (n - 1)

plus :: Nat -> Nat -> Nat
plus x = cata phi
  where
    phi Zero = x
    phi (Succ y) = succ y

mult :: Nat -> Nat -> Nat
mult x = cata phi
  where
    phi Zero = zero
    phi (Succ y) = plus x y

expr :: Nat -> Nat -> Nat
expr x = cata phi
  where
    phi Zero = succ zero
    phi (Succ y) = mult x y

fact :: Nat -> Nat
fact = para phi
  where
    phi Zero = succ zero
    phi (Succ (n, m)) = mult (succ n) m

-- | List a

data ListF a x = Nil | Cons a x deriving (Show, Functor)

type List a = Fix (ListF a)

nil :: List a
nil = In Nil
cons :: a -> List a -> List a
cons x xs = In (Cons x xs)

instance Show a => Show (List a) where
  show x = "(" ++ show (out x) ++ ")"

sum = cata phi
  where
    phi Nil = 0
    phi (Cons a x) = a + x -- !?

length = cata phi
  where
    phi Nil = 0
    phi (Cons a x) = 1 + x -- !?

genList :: Integer -> List Integer
genList = ana psi
  where
    psi n = if n <= 0 then Nil else Cons n (n - 1)

-- insert 4 $ insert 2 $ insert 1 $ insert 3 nil
insert :: Ord a => a -> List a -> List a
insert v = para phi
  where
    phi Nil = cons v nil
    phi (Cons x (xs, ys)) = if v <= x then cons v (cons x xs) else cons x ys
