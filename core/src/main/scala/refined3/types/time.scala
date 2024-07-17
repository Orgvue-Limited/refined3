package refined3.types

import refined3.{Refined, RefinedTypeOps}
import refined3.numeric.Interval.Closed

object time extends time

trait time:
  type Millis = Int Refined Closed[0, 999]
  type Second = Int Refined Closed[0, 59]
  type Minute = Int Refined Closed[0, 59]
  type Hour   = Int Refined Closed[0, 23]
  type Day    = Int Refined Closed[1, 31]
  type Month  = Int Refined Closed[1, 12]

  object Millis extends RefinedTypeOps[Int, Closed[0, 999]]
  object Second extends RefinedTypeOps[Int, Closed[0, 59]]
  object Minute extends RefinedTypeOps[Int, Closed[0, 59]]
  object Hour   extends RefinedTypeOps[Int, Closed[0, 23]]
  object Day    extends RefinedTypeOps[Int, Closed[1, 31]]
  object Month  extends RefinedTypeOps[Int, Closed[1, 12]]