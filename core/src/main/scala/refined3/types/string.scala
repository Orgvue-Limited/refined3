package refined3.types

import refined3.{Refined, RefinedPrimitiveOps}
import refined3.generic.And
import refined3.string.{MinSize, MaxSize, NonEmpty, IPv4 => IsIPv4}

object string extends string

trait string:
  type IPv4                    = String Refined IsIPv4
  type NonEmptyString          = String Refined NonEmpty
  type FiniteString[N]         = String Refined (MinSize[0] And MaxSize[N])
  type NonEmptyFiniteString[N] = String Refined (MinSize[1] And MaxSize[N])

  object IPv4           extends RefinedPrimitiveOps[String, IsIPv4]
  object NonEmptyString extends RefinedPrimitiveOps[String, NonEmpty]

  object FiniteString:
    inline def apply[N <: Int]: RefinedPrimitiveOps[String, MinSize[0] And MaxSize[N]] =
      new RefinedPrimitiveOps[String, MinSize[0] And MaxSize[N]]

  object NonEmptyFiniteString:
    inline def apply[N <: Int]: RefinedPrimitiveOps[String, MinSize[1] And MaxSize[N]] =
      new RefinedPrimitiveOps[String, MinSize[1] And MaxSize[N]]