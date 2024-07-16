package refined.types

import refined.Refined
import refined.generic.And
import refined.string.{MinSize, MaxSize, NonEmpty}

object string extends string

trait string:
  type NonEmptyString          = String Refined NonEmpty
  type FiniteString[N]         = String Refined (MinSize[0] And MaxSize[N])
  type NonEmptyFiniteString[N] = String Refined (MinSize[1] And MaxSize[N])