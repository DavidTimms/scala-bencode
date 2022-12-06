package net.mooli.bencode

import java.nio.charset.StandardCharsets.ISO_8859_1

class BValueTest extends org.scalatest.funspec.AnyFunSpec {

  // Does bencode decoding and encoding work?
  describe("net.mooli.bencode.BValue encode/decode") {
    val tests: List[(String, BValue)] = List(
      // Although these tests mostly check round-tripping, they also have a side-effect of checking
      // that the implicit conversions of tuples to BPairs also works.
      //
      // These test examples are taken from https://wiki.theory.org/BitTorrentSpecification#Bencoding
      "4:spam" -> BString("spam"),
      "0:" -> BString(""),
      "i3e" -> BInt(3),
      "i-3e" -> BInt(-3),
      // FIXME - not tested: "i-0e" and "i03e".
      "l4:spam4:eggse" -> BList("spam", "eggs"),
      "le" -> BList(),
      "d3:cow3:moo4:spam4:eggse" ->
        BDictionary("cow" -> "moo", "spam" -> "eggs"),
      "d4:spaml1:a1:bee" ->
        BDictionary("spam" -> BList("a", "b")),
      // this one also tests keys are sorted correctly: "9:publisher" > "17:publisher-webpage".
      "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee" ->
        BDictionary(
          "publisher.location" -> "home",
          "publisher-webpage" -> "www.example.com",
          "publisher" -> "bob",
        ),
      "de" -> BDictionary(),
      // Now some local tests
      // We *are* decoding UTF-8 correctly, right?
      "2:Â£" -> BString("£"),
      "d2:Â£2:Â£e" -> BDictionary("£" -> "£"),
      // correct sort order for UTF-8 keys
      "d2:GBle2:Â£lee" -> BDictionary("GB" -> BList(), "£" -> BList()),
    )
    for ((printable, tree) <- tests) {
      val bencoded: Array[Byte] = printable getBytes ISO_8859_1
      val prettyBencoded = bencoded map Util.escaped mkString("\"", "", "\"")
      it(s"can decode $prettyBencoded") {
        val i = bencoded.toIterator
        val testTree = BValue.read(i)
        assert(testTree === tree)
        assert(i.hasNext === false, "Did not consume all data")
      }
      it(s"can encode back to $prettyBencoded") {
        val ab = new scala.collection.mutable.ArrayBuilder.ofByte
        tree.write(ab)
        val testPrintable = new String(ab.result, ISO_8859_1)
        assert(testPrintable === printable)
      }

    }
  }

  // Is our toString pretty-printer working correctly?
  describe("net.mooli.bencode.BValue toString") {
    val tests = List[(String, BValue)](
      "BBigInt(-1)" -> BInt(-1),
      """BBinary("Hello \342\202\254uro world")""" -> BString("Hello €uro world"),
      "BList()" -> BList(),
      "BList(1, 2, 3)" -> BList(1, 2, 3),
      "BDictionary()" -> BDictionary(),
      """BDictionary("0" -> "zero", "1" -> 2, "3" -> 4)"""
        -> BDictionary("1" -> 2, "3" -> 4, "0" -> "zero"),
    )
    for ((repr, tree) <- tests) {
      it(s"can toString to $repr") {
        assert(tree.toString === repr)
      }
    }
  }

  // Do we have the correct sort ordering for BBinary objects?
  describe("net.mooli.bencode.BBinary ordering") {
    val tests = List[(String, String)](
      // boring tests
      "" -> "0",
      "0" -> "00",
      "00" -> "1",
      "coder" -> "codex",
      "coder" -> "cover",
      // check sort is *unsigned*
      "\u007f\u00ff" -> "\u0080",
    )
    for ((lowerStr, higherStr) <- tests) {
      import java.nio.charset.StandardCharsets.ISO_8859_1
      val lower = BBinary(lowerStr getBytes ISO_8859_1)
      val higher = BBinary(higherStr getBytes ISO_8859_1)
      it(s"can compare $lower with $higher") {
        assert((lower compare higher) < 0, s"$lower compare $higher is negative")
        assert((higher compare lower) > 0, s"$higher compare $lower is positive")
        assert((lower < higher), s"$lower < $higher")
        assert((higher > lower), s"$higher > $lower")
      }
    }

  }

}
