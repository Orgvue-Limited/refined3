package refined3

import refined3.generic.Not

package object string:
  type NonEmpty = Not[Empty]