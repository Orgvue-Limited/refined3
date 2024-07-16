package refined3

import refined3.generic.{Equal, Not}

package object collection:
  type Empty       = Size[0]
  type NonEmpty    = Not[Empty]
  type Exists[P]   = Not[Forall[Not[P]]]
  type Contains[U] = Exists[Equal[U]]

