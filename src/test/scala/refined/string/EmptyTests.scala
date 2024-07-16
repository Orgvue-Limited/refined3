package refined.string

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EmptyTests extends AnyFunSpec with Matchers {
  it("should work with string literals") {
    val str: String Refined Empty = ""
    val str2: String Refined NonEmpty = "Hello"
    assert(str.value == "" && str2.value == "Hello")
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("""val str: String Refined Empty = "Hello, world!"""")
    assertDoesNotCompile("""val str: String Refined NonEmpty = """"")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[String, Empty]("").isRight)
    assert(Refined.refineV[String, Empty]("Hello, world!").isLeft)


    assert(Refined.refineV[String, NonEmpty]("").isLeft)
    assert(Refined.refineV[String, NonEmpty]("Hello, world!").isRight)
  }
}