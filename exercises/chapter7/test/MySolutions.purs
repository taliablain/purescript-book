module Test.MySolutions where

import Prelude
import Data.AddressBook (Address, Person, PhoneNumber, address, person, phoneNumber)
import Data.String.Regex(Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.AddressBook.Validation (matches)
import Data.Validation.Semigroup (V, invalid)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

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
  show = genericShow