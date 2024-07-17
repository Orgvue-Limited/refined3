package refined3

import refined3.generic.{And, Not}

package object numeric:
  type LessEqual[N]    = Not[Greater[N]]
  type GreaterEqual[N] = Not[Less[N]]
  type Positive        = Greater[0]
  type NonPositive     = Not[Positive]
  type Negative        = Less[0]
  type NonNegative     = Not[Negative]
  type Divisible[N]    = Modulo[N, 0]
  type NonDivisible[N] = Not[Divisible[N]]
  type Even            = Divisible[2]
  type Odd             = Not[Even]

  object Interval:
    type Open[L, H]       = Greater[L] And Less[H]
    type OpenClosed[L, H] = Greater[L] And LessEqual[H]
    type ClosedOpen[L, H] = GreaterEqual[L] And Less[H]
    type Closed[L, H]     = GreaterEqual[L] And LessEqual[H]