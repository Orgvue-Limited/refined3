package refined.string

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MaxSizeTests extends AnyFunSpec with Matchers {
  it("should work with string literals") {
    val str: String Refined MaxSize[15] = "Hello, world!"
    assert(str.value == "Hello, world!")
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("""val str: String Refined MaxSize[10] = "Hello, world!"""")
    assertDoesNotCompile("""val str: String Refined MaxSize[1] = "Yo"""")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[String, MaxSize[8]]("Hello, world!").isLeft)
    assert(Refined.refineV[String, MaxSize[8]]("Hello").isRight)
  }
}