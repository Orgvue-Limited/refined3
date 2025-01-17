package refined3.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class stringTests extends AnyFunSpec with Matchers with string {
  describe("NonEmptyString") {
    it("should typecheck for a valid case") {
      val str: NonEmptyString = "Hello"
      val nonEmptyString = NonEmptyString("Hello")
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("""val str: NonEmptyString = """"")
    }

    it("should validate at runtime correctly") {
      assert(NonEmptyString.from("Hello").isRight)
      assert(NonEmptyString.from("").isLeft)
    }
  }

  describe("FiniteString") {
    it("should typecheck for a valid case") {
      val str: FiniteString[10] = "Hello"
      val finiteString = FiniteString[10]("Hello")
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("""val str: FiniteString[3] = "Hello"""")
    }

    it("should validate at runtime correctly") {
      assert(FiniteString[3].from("you").isRight)
      assert(FiniteString[3].from("Hello").isLeft)
      assert(FiniteString[3].from("").isRight)
    }
  }

  describe("NonEmptyFiniteString") {
    it("should typecheck for a valid case") {
      val str: NonEmptyFiniteString[10] = "Hello"
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("""val str: NonEmptyFiniteString[3] = "Hello"""")
      assertDoesNotCompile("""val str: NonEmptyFiniteString[3] = """"")
    }

    it("should validate at runtime correctly") {
      assert(NonEmptyFiniteString[3].from("you").isRight)
      assert(NonEmptyFiniteString[3].from("Hello").isLeft)
      assert(NonEmptyFiniteString[3].from("").isLeft)
    }
  }

  describe("IPv4"){
    it("should typecheck for a valid case") {
      val ipv4_0: IPv4 = "192.168.40.39"
      val ipv4_1       = IPv4("192.168.40.38")
    }
  }
}