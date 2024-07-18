package refined3.types

import refined3.{Refined, RefinedPrimitiveOps}
import refined3.generic.And
import refined3.numeric.{Interval, Positive, Negative, NonNegative, NonPositive}

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

  type PortNumber   = Int Refined Interval.Closed[0, 65535]

  object PosInt    extends RefinedPrimitiveOps[Int, Positive]
  object NonNegInt extends RefinedPrimitiveOps[Int, NonNegative]
  object NegInt    extends RefinedPrimitiveOps[Int, Negative]
  object NonPosInt extends RefinedPrimitiveOps[Int, NonPositive]

  object PosLong    extends RefinedPrimitiveOps[Long, Positive]
  object NonNegLong extends RefinedPrimitiveOps[Long, NonNegative]
  object NegLong    extends RefinedPrimitiveOps[Long, Negative]
  object NonPosLong extends RefinedPrimitiveOps[Long, NonPositive]

  object PosFloat    extends RefinedPrimitiveOps[Float, Positive]
  object NonNegFloat extends RefinedPrimitiveOps[Float, NonNegative]
  object NegFloat    extends RefinedPrimitiveOps[Float, Negative]
  object NonPosFloat extends RefinedPrimitiveOps[Float, NonPositive]

  object PosDouble    extends RefinedPrimitiveOps[Double, Positive]
  object NonNegDouble extends RefinedPrimitiveOps[Double, NonNegative]
  object NegDouble    extends RefinedPrimitiveOps[Double, Negative]
  object NonPosDouble extends RefinedPrimitiveOps[Double, NonPositive]

  object PortNumber extends RefinedPrimitiveOps[Int, Interval.Closed[0, 65535]]
