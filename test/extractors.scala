package net.mooli.bencode

class ExtractorTests extends org.scalatest.funspec.AnyFunSpec {

  describe("net.mooli.bencode.BInt") {
    it("can extract ints") {
      val BInt(test) = BBigInt("2147483647")
      assert(test === 2147483647)
    }
    it("rejects out-of-range") {
      intercept[MatchError] {
        val BInt(_) = BBigInt("2147483648")
      }
    }
  }

  describe("net.mooli.bencode.BLong") {
    it("can extract longs") {
      val BLong(test) = BBigInt("9223372036854775807")
      assert(test === 9223372036854775807L)
    }
    it("rejects out-of-range") {
      intercept[MatchError] {
        val BLong(_) = BBigInt("9223372036854775808")
      }
    }
  }

  describe("net.mooli.bencode.BString") {
    it("can extract strings") {
      val BString(test) = BBinary(Array[Byte](226 - 256, 130 - 256, 172 - 256))
      assert(test === "€")
    }
    it("rejects out-of-range") {
      intercept[MatchError] {
        val BString(_) = BBinary(Array[Byte](192 - 256)) // 192 is a short sequence
      }
    }
  }

  describe("net.mooli.bencode.BAnyString") {
    it("can extract strings") {
      val BAnyString(test) = BBinary(Array[Byte](226 - 256, 130 - 256, 172 - 256))
      assert(test === "€")
      val BAnyString(test2) = BBinary(Array[Byte](163 - 256))
      assert(test2 === "£")
    }
    it("rejects out-of-range") {
      intercept[MatchError] {
        val BAnyString(_) = BBinary(Array[Byte](129 - 256)) // 129 doesn't exist in Windows-1252
      }
    }
  }

}
