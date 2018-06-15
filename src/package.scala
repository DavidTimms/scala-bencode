// Copyright (c) 2018 Peter Corlett. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice, this list of conditions
//    and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright notice, this list of
//    conditions and the following disclaimer in the documentation and/or other materials provided
//    with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
// WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

package net.mooli

/** Bencode parser and generator.
 *
 *  Bencode is a serialisation format used by BitTorrent. It is specified in
 *  [[http://bittorrent.org/beps/bep_0003.html BEP3]] and some further examples are given at
 *  [[https://wiki.theory.org/BitTorrentSpecification#Bencoding wiki.theory.org]]. Parts of the
 *  BitTorrent wire format are in bencode format, as are `.torrent` files.
 *
 *  There is a superficial resemblance to JSON in that its basic datatypes are integer, string, list
 *  and dictionary, however there are a few odd edge cases due to BitTorrent's Python heritage.
 *  Likewise, this library's data model may resemble JSON-parsing libraries you may have used such
 *  as [[https://app.assembla.com/spaces/liftweb/wiki/JSON_Support lift-json]] in that there is a
 *  supertrait [[BValue]] representing an abstract bencoded value, and subclasses for each of the
 *  concrete bencode types as follows:
 *
 *  - [[BBigInt]] is the "integer" type which boxes a [[scala.math.BigInt BigInt]], an
 *    arbitary-length signed integer that directly maps to Python's `long` type. Because bigints are
 *    tedious to use, [[BInt]] and [[BLong]] provide useful factories/extractors so that you can
 *    pretend that you are really working with regular fixed-size [[scala.Int Int]]s and
 *    [[scala.Long Long]]s.
 *
 *  - [[BBinary]] is the "string" type which boxes a private [[scala.Array Array[Byte]]], a sequence
 *    of bytes that directly maps to Python's `str` type. This can be either binary data or a
 *    string, in which case the binary data is UTF-8 encoded text. Again, since you almost always
 *    want to work with Scala [[scala.Predef.String String]]s instead, a [[BString]]
 *    factory/extractor exists.
 *
 *  - [[BList]] is the "list" type which boxes a [[scala.Vector Vector[BValue]]], an indexed
 *    sequence of arbitrary values.
 *
 *  - [[BDictionary]] is the "dictionary" type which boxes a [[scala.Predef.Map Map[BBinary,
 *    BValue]]], a mapping from ''strings'' to values.
 *
 *  You can construct and pattern-match the various types by either the real classes or the helpers
 *  like so:
 *  {{{
 *  scala> val answer = BInt(42)
 *  answer: net.mooli.bencode.BBigInt = BBigInt(42)
 *
 *  scala> val i = answer match { case BLong(x) => x }
 *  i: Long = 42
 *
 *  scala> val big = BLong(4294967296L)
 *  big: net.mooli.bencode.BBigInt = BBigInt(4294967296)
 *
 *  scala> big match { case BInt(x) => x }
 *  scala.MatchError: BBigInt(4294967296) (of class net.mooli.bencode.BBigInt)
 *
 *  scala> val torrent = BDictionary("announce" -> "http://example.com/", "info" -> BDictionary("piece length" -> 65536))
 *  torrent: net.mooli.bencode.BDictionary = BDictionary("announce" -> "http://example.com/", "info" -> BDictionary("piece length" -> 65536))
 *  }}}
 *
 *  The last example hints at implicit conversions, and this is indeed the case. There is the
 *  [[ToBValue]] typeclass which support arbitrary conversion to a [[BValue]], and its companion
 *  object contains useful conversions from standard Scala types such as [[ToBValue.fromString]].
 *  You are encouraged to add to this typeclass to support converting your own custom types into
 *  bencode.
 *
 *  [[ToBValue]] is sufficient to create nearly everything, but sadly(?!) Scala implicits are not
 *  magical enough to do the multiple levels of indirection required to construct a dictionary from
 *  key/value pairs. To achieve that, there is [[BDictionary.apply]] which takes multiple [[BPair]]
 *  parameters (a `BPair` isa [[scala.Tuple2 (BBinary,BValue)]]), which also makes use of the
 *  [[ToBBinary]] typeclass. In practice, you should never need to care about the existence of
 *  [[BPair]] or [[ToBBinary]], but are warned about them here for cases when the Scala compiler
 *  loses its mind and gives confusing error messages.
 *
 *  The final, and arguably the most important part of this library, is the actual serialisation and
 *  deserialisation of bencoded data.
 *
 *  To serialise, you call [[BValue.write]] and give it a [[BBuilder]]. There is a corresponding
 *  [[ToBBuilder]] typeclass, with some useful precanned conversions in its [[ToBBuilder$ companion
 *  object]]:
 *
 *  {{{
 *  scala> val ab = new collection.mutable.ArrayBuilder.ofByte
 *  ab: collection.mutable.ArrayBuilder.ofByte = ArrayBuilder.ofByte
 *
 *  scala> BValue("hello").write(ab)
 *  scala> val serialised = ab.result
 *  serialised: Array[Byte] = Array(53, 58, 104, 101, 108, 108, 111)
 *  scala> new String(serialised)
 *  res1: String = 5:hello
 *  }}}
 *
 *  To deserialise, one passes a [[BIterator]] to [[BValue.read]], which returns a [[BValue]]. Once
 *  again, there is a [[ToBIterator]] typeclass that can be used for implicit conversions, and whose
 *  [[ToBIterator$ companion object]] also provides a few precanned conversions:
 *
 *  {{{
 *  scala> BValue.read(serialised)
 *  res2: net.mooli.bencode.BValue = BBinary("hello")
 *  }}}
 *
 *  @groupname bvalue BValue hierarchy
 *
 *  @groupname bfactory BValue factories and/or extractors
 *
 *  @groupdesc bfactory
 *
 *  The four core bencode types of [[BBigInt]], [[BBinary]], [[BList]] and [[BDictionary]] are often
 *  less than convenient to use directly, so as well as the standard factories and extractors that
 *  come for free with the core types, several extra generally-useful extra factories and/or
 *  extractors are included here to make the library easier to use.
 *
 *  @groupname parsing Reading and decoding bencoded data
 *
 *  @groupname generating Encoding and writing bencoded data
 */
package object bencode {

  import scala.language.implicitConversions

  /** The type of the key/value pair in a BDictionary.
   */
  type BPair = (BBinary, BValue)

  /** Generic conversion from `(T, U)` to a `BPair` via both [[ToBBinary]] and [[ToBValue]]. This
   *  handles e.g. `BDictionary("x" -> "y")` via `ToBBinary.fromString` and `ToBValue.fromString`.
   */
  implicit def toBPair[T: ToBBinary, U: ToBValue](tuple: (T, U)): BPair =
    (implicitly[ToBBinary[T]] apply (tuple._1), implicitly[ToBValue[U]] apply (tuple._2))

  /** Generic conversion from `(T, BValue)` to a `BPair` via [[ToBBinary]]. This handles e.g.
   *  `BDictionary("x" -> BList())`.
   */
  implicit def toBPair[T: ToBBinary](tuple: (T, BValue)): BPair =
    (implicitly[ToBBinary[T]] apply tuple._1, tuple._2)

  /** Generic conversion from `(BBinary, T)` to a `BPair` via [[ToBValue]]. This is an unlikely edge
   *  case, and handles e.g. `BDictionary(BString("x") -> "y").
   */
  implicit def toBPair[T: ToBValue](tuple: (BBinary, T)): BPair =
    (tuple._1, implicitly[ToBValue[T]] apply tuple._2)
}
