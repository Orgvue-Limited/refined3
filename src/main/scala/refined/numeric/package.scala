package refined

import refined.generic.Not

package object numeric:
  type LessEqual[N]    = Not[Greater[N]]
  type GreaterEqual[N] = Not[Less[N]]
  type Positive        = Greater[0]
  type NonPositive     = Not[Positive]
  type Negative        = Less[0]
  type NonNegative     = Not[Negative]