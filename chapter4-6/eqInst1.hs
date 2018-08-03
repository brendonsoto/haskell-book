-- We're declaring a datatype here
data TisAnInteger =
-- We're saying "TisAnInteger" is composed of a "TisAn" and an Integer
  TisAn Integer

-- Here we're implementing the typeclass "Eq" in "TisAnInteger"
instance Eq TisAnInteger where
-- Here we're defining the equivalence of two TisAns and integers
-- "int" & "int'" are used as placeholders since we know already that Integer implements the "Eq" typeclass
-- In other words, we know we can compare Integers based on equality already
-- We don't have any means of comparing TisAn so we're just leaving the made-up type there
  (==) (TisAn int) (TisAn int') =
       int == int'
