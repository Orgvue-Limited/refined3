import scala.compiletime.ops.any.ToString

import scala.compiletime.constValue

import scala.reflect.ClassTag

package object refined3:
  transparent inline def stringOf[A] =
    constValue[ToString[A]]

  transparent inline def constValueOf[A] =
    constValue[A]