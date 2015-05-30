------------------------------------------------------------------------
-- The Agda standard library
--
-- Showing booleans
------------------------------------------------------------------------

module Data.Bool.Show where

open import Data.Bool
open import Data.String hiding (show)

show : Bool → String
show true  = "true"
show false = "false"
