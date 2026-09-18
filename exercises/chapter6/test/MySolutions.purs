module Test.MySolutions where

import Data.Foldable
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prim.Ordering (EQ, GT, LT)


-- Note to reader: Add your solutions to this file

-- why does Point need to be a newtype? 

newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance Show Point where 
     show (Point {x, y}) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex 
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance Show Complex where 
 show (Complex { real, imaginary }) =
    show real <> sign <> show imaginary <> "i"
    where
    sign = if imaginary >= 0.0 then "+" else ""
  
derive newtype instance eqComplex :: Eq Complex 

{-
instance Ring Complex where 
  sub (Complex a)(Complex b) =
    Complex { real: a.real - b.real, imaginary: a.imaginary - b.imaginary }

-}
instance Semiring Complex where 
  zero = Complex { real: 0.0, imaginary: 0.0 }
  one = Complex {real: 1.0, imaginary: 0.0 }
  add (Complex a) (Complex b) = 
    Complex { real: a.real + b.real, imaginary: a.imaginary + b.imaginary }
  mul (Complex a)(Complex b)=
    Complex 
      { real: a.real * b.real - a.imaginary * b.imaginary, imaginary: a.real * b.imaginary + a.imaginary * b.real}
  
derive newtype instance ringComplex :: Ring Complex

-- derive via Generic a Show instance for Shape 

data Shape 
  = Circle Point Number 
  | Rectangle Point Number Number 
  | Line Point Point
  | Text Point String 

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where 
  show = genericShow


data NonEmpty a = NonEmpty a (Array a)

derive instance genericNonEmpty :: Generic (NonEmpty a) _

instance showNonEmpty :: Show a => Show (NonEmpty a) where 
  show = genericShow

derive instance Eq a => Eq (NonEmpty a)



instance semigroupNonEmpty :: Semigroup (NonEmpty a) where 
  append (NonEmpty a ax)(NonEmpty b bx) = NonEmpty a (ax <> [b] <> bx)

-- write a functor instance for NonEmpty 

-- map :: forall a b. (a -> b) -> NonEmpty a -> NonEmpty b

instance functorNonEmpty :: Functor NonEmpty where 
  map f (NonEmpty a ax) = NonEmpty (f a) (f <$> ax)

-- given any type a with an instance of Ord, we can add a new "infinite" value
-- that is greater than any other value:
-- data Extended a = Infinite | Finite a 
-- write an Ord instance for Extended a that reuses the Ord instance for a. 

data Extended a = Infinite | Finite a
instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Infinite)(Infinite) = EQ
  compare (Infinite)(Finite _) = GT
  compare (Finite _)(Infinite) = LT 
  compare (Finite a)(Finite b) = compare (a)(b)

instance eqExtended :: Eq a => Eq (Extended a) where 
  eq (Infinite)(Infinite) = true
  eq (Infinite)(Finite _) = false 
  eq (Finite _)(Infinite) = false
  eq (Finite a)(Finite b) = false