# scala-bencode

This is (yet another) bencode library for Scala. It's a bit more polished than
most, and includes novel features such as documentation and tests, and the
ability to serialise *to* bencode. Crazy, right?

Extensive usage documentation is in the scaladoc, but here's an example as a
taster:

```Scala

// read a torrent file into a BValue:
val torrent = BValue.read(Files.readAllBytes(torrentPath))
torrent match {
  case BDictionary(kvs) => ???
}

// serialise into a byte array:
val array = {
  val builder = Array.newBuilder[Byte]
  torrent.write(builder)
  builder.result
}

```

This library is not currently being published to e.g. Sonatype. You are instead
encouraged to just cut-and-paste the code directory into your own project,
ideally after changing the package namespace. Please drop me an email at
github@cabal.org.uk if you do that, so I can advise you of any major changes in
the future which you may wish to incorporate into your own copy.

