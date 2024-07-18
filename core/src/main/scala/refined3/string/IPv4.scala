package refined3
package string

import quoted.{Expr => ExprQ, Quotes}

final case class IPv4()

object IPv4:
  private val regex    = "^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$".r.pattern
  private val maxOctet = 255

  transparent implicit inline def ipv4Expr[T]: Expr[T, IPv4] =
    Expr[T, IPv4]("\"" + stringOf[T] + "\" is IPv4")

  transparent implicit inline def ipv4Proof[T <: String]: Proof[T, IPv4] =
    inline if isIPv4(stringOf[T])
    then Proof.success[T, IPv4]
    else Proof.failure[T, IPv4]

  implicit inline def IPv4Validate[T <: String]: Validate[T, IPv4] =
    (base: T) => predicate(base)

  implicit inline def IPv4Show[T]: Show[T, IPv4] =
    (base: T) => s"\"$base\" is IPv4"

  private def predicate(str: String): Boolean =
    val matcher = regex.matcher(str)
    matcher.find() && matcher.matches() && {
      val octet1 = matcher.group(1).toInt
      val octet2 = matcher.group(2).toInt
      val octet3 = matcher.group(3).toInt
      val octet4 = matcher.group(4).toInt
      (octet1 <= maxOctet) && (octet2 <= maxOctet) && (octet3 <= maxOctet) && (octet4 <= maxOctet)
    }

  private transparent inline def isIPv4(inline v: String): Boolean =
    ${ isIPv4Code('v) }

  private def isIPv4Code(v: ExprQ[String])(using Quotes): ExprQ[Boolean] =
    val res = predicate(v.valueOrAbort)
    ExprQ(res)