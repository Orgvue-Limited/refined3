package refined

import refined.generic.Not

package object collection:
  type Empty    = Size[0]
  type NonEmpty = Not[Empty]
