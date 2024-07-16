package refined3.string

import refined3.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MinSizeTests extends AnyFunSpec with Matchers {
  it("should work with string literals") {
    val str: String Refined MinSize[5] = "Hello, world!"
    assert(str.value == "Hello, world!")
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("""val str: String Refined MinSize[100] = "Hello, world!"""")
    assertDoesNotCompile("""val str: String Refined MinSize[1] = """"")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[String, MinSize[8]]("Hello, world!").isRight)
    assert(Refined.refineV[String, MinSize[8]]("Hello").isLeft)
  }
}