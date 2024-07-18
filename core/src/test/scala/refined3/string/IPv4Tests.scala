package refined3.string

import refined3.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class IPv4Tests extends AnyFunSpec with Matchers {
  it("should work with string literals") {
    val str: String Refined IPv4 = "192.168.40.39"
    assert(str.value == "192.168.40.39")
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("""val ip: String Refined IPv4 = "1921.168.40.39"""")
    assertDoesNotCompile("""val ip: String Refined IPv4 = "192.568.40.39"""")
    assertDoesNotCompile("""val ip: String Refined IPv4 = "192.168.440.39"""")
    assertDoesNotCompile("""val ip: String Refined IPv4 = "192.168.40.339"""")
    assertDoesNotCompile("""val ip: String Refined IPv4 = """"")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[String, IPv4]("192.168.40.39").isRight)
    assert(Refined.refineV[String, IPv4]("1192.168.40.39").isLeft)
  }
}