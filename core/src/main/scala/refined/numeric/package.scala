package refined

import refined.generic.{And, Not}

package object numeric:
  type LessEqual[N]    = Not[Greater[N]]
  type GreaterEqual[N] = Not[Less[N]]
  type Positive        = Greater[0]
  type NonPositive     = Not[Positive]
  type Negative        = Less[0]
  type NonNegative     = Not[Negative]

  object Interval:
    type Open[L, H]       = Greater[L] And Less[H]
    type OpenClosed[L, H] = Greater[L] And LessEqual[H]
    type ClosedOpen[L, H] = GreaterEqual[L] And Less[H]
    type Closed[L, H]     = GreaterEqual[L] And LessEqual[H]