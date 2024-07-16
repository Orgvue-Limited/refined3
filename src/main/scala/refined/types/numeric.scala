package refined.types

import refined.{Refined, RefinedTypeOps}
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

  object PosInt    extends RefinedTypeOps[Int, Positive]
  object NonNegInt extends RefinedTypeOps[Int, NonNegative]
  object NegInt    extends RefinedTypeOps[Int, Negative]
  object NonPosInt extends RefinedTypeOps[Int, NonPositive]

  object PosLong    extends RefinedTypeOps[Long, Positive]
  object NonNegLong extends RefinedTypeOps[Long, NonNegative]
  object NegLong    extends RefinedTypeOps[Long, Negative]
  object NonPosLong extends RefinedTypeOps[Long, NonPositive]

  object PosFloat    extends RefinedTypeOps[Float, Positive]
  object NonNegFloat extends RefinedTypeOps[Float, NonNegative]
  object NegFloat    extends RefinedTypeOps[Float, Negative]
  object NonPosFloat extends RefinedTypeOps[Float, NonPositive]

  object PosDouble    extends RefinedTypeOps[Double, Positive]
  object NonNegDouble extends RefinedTypeOps[Double, NonNegative]
  object NegDouble    extends RefinedTypeOps[Double, Negative]
  object NonPosDouble extends RefinedTypeOps[Double, NonPositive]
