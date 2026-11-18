module Test.MySolutions where

import Prelude

import Data.AddressBook (Address, Person, PhoneNumber, address)
import Data.AddressBook.Validation (matches, validatePhoneNumbers, validateAddress, nonEmpty)
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Map.Internal (Map)
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Validation.Semigroup (V)
import Data.Maybe (Maybe(..))

-- Note to reader: Add your solutions to this file

{- write a regular expression stateRegex :: Regex to check that a string 
only contains two alphabetic characters. Hint: See the source code for 
phoneNumberRegex
-}
stateRegex :: Regex
stateRegex = unsafeRegex "^[A-Za-z]{2}$" noFlags

{- write a regular expression nonEmptyRegex :: Regex to check that a string
is not entirely whitespace.
-}

nonEmptyRegex :: Regex 
nonEmptyRegex = unsafeRegex "[^\\s]" noFlags

{- write a function validateAddressImproved that is similar to validateAddress, but 
uses the above stateRegex to validate the state field and nonEmptyRegex to validate
the street and city fields. Hint: see the source for validatePhoneNumber for an example
of how to use matches
-}

validateAddressImproved :: Address -> V (Array String) Address 
validateAddressImproved add = ado 
  street <- matches "Street" nonEmptyRegex add.street
  city <- matches "City" nonEmptyRegex add.city
  state <- matches "State" stateRegex add.state
  in address street city state


{- write Eq and Show instances for the following binary tree data structure: -}

data Tree a = Leaf | Branch (Tree a) a (Tree a)

-- if a has an Eq instance, then derive an Eq instance for Tree a
derive instance Eq a => Eq (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show t = genericShow t

instance Functor Tree where
  map _ Leaf = Leaf
  map f (Branch l a r) = Branch (map f l) (f a) (map f r)

instance Foldable Tree where
  foldr _ z Leaf = z
  foldr f z (Branch l a r) = foldr f (f a (foldr f z r)) l

  foldl _ z Leaf = z
  foldl f z (Branch l a r) = foldl f (f (foldl f z l) a) r

  foldMap _ Leaf = mempty
  foldMap f (Branch l a r) = foldMap f l <> f a <> foldMap f r

{-write a Traversable instance for Tree a, which combines side-effects left to right-}
instance Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch l a r) =
    Branch <$> traverse f l <*> f a <*> traverse f r

  sequence Leaf = pure Leaf
  sequence (Branch l a r) =
    Branch <$> sequence l <*> a <*> sequence r

{- write a function traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b) 
that performs a pre-order traversal of the tree. This means the order of effect execution is root-left-right,
instead of left-root-right as was done for the previous in-order traverse exercise. Hint: no additional instances
need to be defined, and you don't need to call any of the functions defined earlier. Applicative do notation is the 
easiest way to write this function.-}

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)

traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch l a r) = ado
  a' <- f a
  l' <- traversePreOrder f l
  r' <- traversePreOrder f r
  in Branch l' a' r'

{-write a function traversePostOrder that performs a post-order traversal of the tree where 
effects are executed left-right-root -}


traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)

traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch l a r) = ado 
  l' <- traversePostOrder f l 
  r' <- traversePostOrder f r
  a' <- f a
  in Branch l' a' r'

{-create a new version of the Person type where the homeAddress field is optional (using Maybe). Then write
a new version of validatePerson (renamed as validatePersonOptionalAddress) to validate this new Person. Hint: use 
traverse to validate a field of type Maybe a.-}

type NewPerson = { firstName :: String, lastName:: String, phones :: Array PhoneNumber, homeAddress :: Maybe Address }

type Errors
  = Array String

validatePersonOptionalAddress :: NewPerson -> V Errors NewPerson
validatePersonOptionalAddress p = ado
  firstName <- nonEmpty "First Name" p.firstName
  lastName <- nonEmpty "Last Name" p.lastName
  phones <- validatePhoneNumbers "Phone Number" p.phones
  homeAddress <- traverse (validateAddress) p.homeAddress
  in { firstName, lastName, phones, homeAddress }

{- write a function sequenceUsingTraverse which behaves like sequence, but is written in terms of traverse-}
sequenceUsingTraverse :: forall t a m. Traversable t => Applicative m => t (m a) -> m (t a)
-- sequence swaps the layers of traverse 
sequenceUsingTraverse = traverse identity 

{- write a function traverseUsingSequence which behaves like traverse but is written in terms of sequence
traverseUsingSequence :: forall t a b m. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence = 
-}
