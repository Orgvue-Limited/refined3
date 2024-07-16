package refined.string

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class StartsWithTests extends AnyFunSpec with Matchers {
  it("should work with string literals") {
    val str: String Refined StartsWith["Hello"] = "Hello, world!"
    assert(str.value == "Hello, world!")
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("""val str: String Refined StartsWith["What?"] = "Hello, world!"""")
    assertDoesNotCompile("""val str: String Refined StartsWith["What?"] = """"")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[String, StartsWith["Hello"]]("Hello, world!").isRight)
    assert(Refined.refineV[String, StartsWith["What?"]]("Hello, world!").isLeft)
  }
}