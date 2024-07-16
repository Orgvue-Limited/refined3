package refined.types

import refined.Refined
import refined.generic.And
import refined.numeric.{Positive, Negative, NonNegative, NonPositive}

object numeric extends numeric

trait numeric:
  type PosInt     = Int Refined Positive
  type NonNegInt  = Int Refined NonNegative
  type NegInt     = Int Refined Negative
  type NonPosInt  = Int Refined NonPositive

  type PosLong    = Long Refined Positive
  type NonNegLong = Long Refined NonNegative
  type NegLong    = Long Refined Negative
  type NonPosLong = Long Refined NonPositive

  type PosFloat    = Float Refined Positive
  type NonNegFloat = Float Refined NonNegative
  type NegFloat    = Float Refined Negative
  type NonPosFloat = Float Refined NonPositive

  type PosDouble    = Double Refined Positive
  type NonNegDouble = Double Refined NonNegative
  type NegDouble    = Double Refined Negative
  type NonPosDouble = Double Refined NonPositive