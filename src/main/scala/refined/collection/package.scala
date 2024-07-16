package refined

import refined.generic.{Equal, Not}

package object collection:
  type Empty       = Size[0]
  type NonEmpty    = Not[Empty]
  type Exists[P]   = Not[Forall[Not[P]]]
  type Contains[U] = Exists[Equal[U]]

