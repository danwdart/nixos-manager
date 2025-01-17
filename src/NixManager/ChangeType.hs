{-|
  Description: Determine if the user made any changes which will have to be applied.
 -}
{-# LANGUAGE DeriveGeneric #-}
module NixManager.ChangeType
  ( ChangeType(..)
  )
where

import           GHC.Generics (Generic)

-- | Avoid boolean blindness by using this enum instead.
data ChangeType = NoChanges -- ^ No changes to apply
                | Changes   -- ^ There are changes to apply
                deriving (Eq, Bounded, Enum, Generic)
