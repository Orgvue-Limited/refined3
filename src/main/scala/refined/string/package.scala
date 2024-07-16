package refined

import refined.generic.Not

package object string:
  type NonEmpty = Not[Empty]