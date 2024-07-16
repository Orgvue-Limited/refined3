package refined3.types

import refined3.{Refined, RefinedTypeOps}
import refined3.generic.And
import refined3.string.{MinSize, MaxSize, NonEmpty}

object string extends string

trait string:
  type NonEmptyString          = String Refined NonEmpty
  type FiniteString[N]         = String Refined (MinSize[0] And MaxSize[N])
  type NonEmptyFiniteString[N] = String Refined (MinSize[1] And MaxSize[N])

  object NonEmptyString extends RefinedTypeOps[String, NonEmpty]

  object FiniteString:
    inline def apply[N <: Int]: RefinedTypeOps[String, MinSize[0] And MaxSize[N]] =
      new RefinedTypeOps[String, MinSize[0] And MaxSize[N]]

  object NonEmptyFiniteString:
    inline def apply[N <: Int]: RefinedTypeOps[String, MinSize[1] And MaxSize[N]] =
      new RefinedTypeOps[String, MinSize[1] And MaxSize[N]]