module Util.Integral where

instance Integral Float where
  quotRem a b = (fab, (ab - fab)*b)
    where
      ab = a/b
      fab = floor ab
  toInteger = floor

instance Integral Double where
  quotRem a b = (fab, (ab - fab)*b)
    where
      ab = a/b
      fab = floor ab
  toInteger = floor

