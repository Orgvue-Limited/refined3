package refined.string

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EndsWithTests extends AnyFunSpec with Matchers {
  it("should work with string literals") {
    val str: String Refined EndsWith["world!"] = "Hello, world!"
    assert(str.value == "Hello, world!")
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("""val str: String Refined EndsWith["What?"] = "Hello, world!"""")
    assertDoesNotCompile("""val str: String Refined EndsWith["What?"] = """"")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[String, EndsWith["world!"]]("Hello, world!").isRight)
    assert(Refined.refineV[String, EndsWith["What?"]]("Hello, world!").isLeft)
  }
}