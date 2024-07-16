package refined3.types

import refined3.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class numericTests extends AnyFunSpec with Matchers with numeric {
  describe("PosInt") {
    it("should typecheck for a valid case") {
      val num: PosInt = 5
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: PosInt = -1")
    }

    it("should validate at runtime correctly") {
      assert(PosInt.from(5).isRight)
      assert(PosInt.from(-1).isLeft)
    }
  }

  describe("NegInt") {
    it("should typecheck for a valid case") {
      val num: NegInt = -5
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: NegInt = 1")
    }

    it("should validate at runtime correctly") {
      assert(NegInt.from(5).isLeft)
      assert(NegInt.from(-1).isRight)
    }
  }

  describe("PosLong") {
    it("should typecheck for a valid case") {
      val num: PosLong = 5L
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: PosLong = -1L")
    }

    it("should validate at runtime correctly") {
      assert(PosLong.from(5L).isRight)
      assert(PosLong.from(-1L).isLeft)
    }
  }

  describe("NegLong") {
    it("should typecheck for a valid case") {
      val num: NegLong = -5L
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: NegLong = 1L")
    }

    it("should validate at runtime correctly") {
      assert(NegLong.from(5L).isLeft)
      assert(NegLong.from(-1L).isRight)
    }
  }

  describe("PosFloat") {
    it("should typecheck for a valid case") {
      val num: PosFloat = 5F
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: PosFloat = -1F")
    }

    it("should validate at runtime correctly") {
      assert(PosFloat.from(5F).isRight)
      assert(PosFloat.from(-1F).isLeft)
    }
  }

  describe("NegFloat") {
    it("should typecheck for a valid case") {
      val num: NegFloat = -5F
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: NegFloat = 1F")
    }

    it("should validate at runtime correctly") {
      assert(NegFloat.from(5F).isLeft)
      assert(NegFloat.from(-1F).isRight)
    }
  }

  describe("PosDouble") {
    it("should typecheck for a valid case") {
      val num: PosDouble = 5D
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: PosDouble = -1D")
    }

    it("should validate at runtime correctly") {
      assert(PosDouble.from(5D).isRight)
      assert(PosDouble.from(-1D).isLeft)
    }
  }

  describe("NegDouble") {
    it("should typecheck for a valid case") {
      val num: NegDouble = -5D
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: NegDouble = 1D")
    }

    it("should validate at runtime correctly") {
      assert(NegDouble.from(5D).isLeft)
      assert(NegDouble.from(-1D).isRight)
    }
  }

  describe("PortNumber") {
    it("should typecheck for a valid case") {
      val port: PortNumber = 9001
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: PortNumber = -1")
      assertDoesNotCompile("val num: PortNumber = 165535")
    }

    it("should validate at runtime correctly") {
      assert(PortNumber.from(8001).isRight)
      assert(PortNumber.from(-1).isLeft)
    }
  }
}