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

package net.mooli.bencode
import Util._

/** Supertrait for bencode AST.
 *
 *  @group bvalue
 */
sealed trait BValue extends Any {

  /** Serialises this object by writing it to the given BBuilder. */
  def write(builder: BBuilder): BBuilder

  /** A shorter "representation" form, used for prettier .toString output */
  protected[bencode] def repr: String = this.toString

}

/** Factory for creating a [[BValue]] from bencoded data.
 *
 *  @group parsing
 */
object BValue {
  import scala.language.implicitConversions

  /** Implicit any-to-BValue factory via ToBValue typeclass. */
  implicit def apply[T: ToBValue](value: T): BValue = implicitly[ToBValue[T]] apply value

  /** Reads bencoded data from the BIterator and deserialises it into a BValue. */
  def read(it: BIterator): BValue = {
      def dictionary(next: Byte): BDictionary = {
        val builder = Map.newBuilder[BBinary, BValue]
          @annotation.tailrec def loop(next: Byte): BDictionary = next match {
            case 'e'   => BDictionary(builder.result)
            case other => builder += string(other) -> value(it.next); loop(it.next)
          }
        loop(next)
      }
      def integer(next: Byte, sentinel: Byte) = {
        val builder = new StringBuilder
          @annotation.tailrec def loop(next: Byte): BBigInt = next match {
            case `sentinel` => BBigInt(builder.result)
            case other      => builder += other.toChar; loop(it.next)
          }
        loop(next)
      }
      def list(next: Byte): BList = {
        val builder = Vector.newBuilder[BValue]
          @annotation.tailrec def loop(next: Byte): BList = next match {
            case 'e'   => BList(builder.result)
            case other => builder += value(other); loop(it.next)
          }
        loop(next)
      }
      def string(next: Byte): BBinary = integer(next, ':') match {
        case BInt(length) => BBinary(it, length)
        case other        => throw new DecodeException(s"string length '$other' is not an integer")
      }
      def value(next: Byte) = next match {
        case 'd'                   => dictionary(it.next)
        case 'i'                   => integer(it.next, 'e')
        case 'l'                   => list(it.next)
        case d if d.toChar.isDigit => string(d)
        case other                 => throw new DecodeException(s"invalid type '${escaped(other)}'")
      }
    value(it.next)
  }
}

/** A bencoded arbitrary-length integer.
 *
 *  @group bvalue
 */
final case class BBigInt(value: BigInt)
  extends AnyVal with BValue {

  def write(builder: BBuilder): BBuilder = builder += 'i' ++= encodeUTF8(value.toString) += 'e'

  // repr doesn't want the surrounding "BBigInt(...)" of toString
  override def repr: String = value.toString

}

/** Factory/extrator for [[BBigInt]]s.
 *
 *  @group bfactory
 */
object BBigInt {

  def apply(x: String): BBigInt = BBigInt(BigInt(x))

}

/** A bencoded string or binary data.
 *
 *  @group bvalue
 */
final class BBinary private (private val value: Array[Byte])
  extends BValue with scala.math.Ordered[BBinary] {

  // Members declared in net.mooli.bencode.BValue
  def write(builder: BBuilder): BBuilder =
    builder ++= encodeUTF8(value.length.toString) += ':' ++= value.toArray

  // Members declared in scala.math.Ordered
  def compare(that: BBinary): Int = {
    val left = this.value
    val right = that.value
    val leftSize = left.length
    val rightSize = right.length
      @annotation.tailrec @inline
      def loop(i: Int): Int = i match {
        case `leftSize` | `rightSize` => leftSize - rightSize
        case _ => (left(i) & 255) - (right(i) & 255) match {
          case 0        => loop(i + 1)
          case compared => compared
        }
      }
    loop(0)
  }

  // because we're a fake case class
  override def hashCode: Int = value.hashCode
  override def equals(that: Any): Boolean = that match {
    case that: BBinary => (this compare that) == 0
    case _             => false
  }

  // because "BBinary([L@12345)" isn't terribly helpful.
  /** Pretty-print bencoded string */
  override def toString: String = "BBinary("+this.repr+")"

  // repr doesn't want the surrounding "BBinary(...)"
  override def repr: String = if (value.size > 256) {
    value take 250 map escaped mkString ("\"", "", s"""\"...(${value.size - 250} more octets)""")
  } else {
    value map escaped mkString ("\"", "", "\"")
  }

  def toArray: Array[Byte] = BBinary.dup(value)
}

/** Factory/extractor for [[BBinary]]s.
 *
 *  @group bfactory
 */
object BBinary {
  import scala.reflect.ClassTag

  private def dup[T: ClassTag](src: Array[T]): Array[T] = {
    val dest = new Array[T](src.length)
    src.copyToArray(dest)
    dest
  }

  def apply(bs: Array[Byte]): BBinary = new BBinary(dup(bs))
  def apply(i: BIterator, length: Int): BBinary = BBinary(i.nextSeq(length))
  def unapply(bb: BBinary): Some[Array[Byte]] = Some(bb.toArray)
}

/** A bencoded dictionary/`Map`.
 *
 *  @group bvalue
 */
final case class BDictionary(value: Map[BBinary, BValue])
  extends AnyVal with BValue {

  private[this] def toSortedTuples = value.toSeq sortBy (_._1)

  def write(builder: BBuilder): BBuilder = {
    builder += 'd'
    this.toSortedTuples foreach { case (key, value) => key.write(builder); value.write(builder) }
    builder += 'e'
  }

  override def toString: String = this.toSortedTuples map {
    case (key, value) => s"${key.repr} -> ${value.repr}"
  } mkString ("BDictionary(", ", ", ")")

}

/** Factory/extractor for [[BDictionary]]s.
 *
 *  [[BDictionary#apply]] uses implicit resolution to allow natural syntax like this:
 *  {{{
 *  BDictionary("foo" -> 1, "bar" -> "baz")
 *  }}}
 *
 *  Unfortunately, there is an oddity/bug in up to at least Scala 2.12 where it will not use
 *  an @implicitNotFound() for the two-stage implicit conversion via BPair, and you will get a
 *  confusing error like this instead:
 *
 *  {{{
 *  [error] .../Foo.scala:NN: type mismatch;
 *  [error]  found   : (Int, Int)
 *  [error]  required: net.mooli.bencode.BPair
 *  [error]       "BDictionary(1 -> 2)" -> BDictionary("1" -> 2, "3" -> 4, 5 -> 6)
 *  [error]
 *  }}}
 *
 *  In this specific example, it is because the integer 5 is not a valid key in a BDictionary. A
 *  dictionary key must be a BBinary, and there is no implicit ToBBinary[Int] conversion available.
 *  Similarly, dictionary values must be BValues, and require a corresponding implicit ToBValue
 *  conversion.
 *
 *  @group bfactory
 */
object BDictionary {
  def apply(pairs: BPair*): BDictionary = new BDictionary(pairs.toMap)
}

/** A bencoded list/`Vector`.
 *
 *  @group bvalue
 */
final case class BList(value: Vector[BValue])
  extends AnyVal with BValue {

  def write(builder: BBuilder): BBuilder = {
    builder += 'l'
    value foreach { _.write(builder) }
    builder += 'e'
  }

  override def toString: String = value map { _.repr } mkString ("BList(", ", ", ")")

}

/** Factory/extractor for [[BList]]s.
 *
 *  @group bfactory
 */
object BList {
  def apply(values: BValue*): BList = new BList(values.toVector)
}
