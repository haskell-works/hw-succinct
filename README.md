# hw-succinct
[![v0.0-branch](https://circleci.com/gh/haskell-works/hw-succinct/tree/v0.0-branch.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-succinct/tree/v0.0-branch)

Conduits for tokenizing streams.

`hw-succinct` is a succinct JSON parsing library.  It uses succinct data-structures to allow traversal of
large JSON strings with minimal memory overhead.

It is currently considered experimental.

For an example, see [`app/Main.hs`](../master/app/Main.hs)

## Prerequisites
* Install `haskell-stack`.
* Install `hlint` (eg. `stack install hlint`)

## Building

Run the following in the shell:

    git clone git@github.com:haskell-works/hw-succinct.git
    cd hw-succinct
    stack setup
    stack build
    stack test
    stack ghci --ghc-options -XOverloadedStrings \
      --main-is hw-succinct:exe:hw-succinct-example

## Memory benchmark

### Parsing large Json files in Scala with Argonaut

          S0U       EU           OU       MU     CCSU CMD
    --------- --------- ----------- -------- -------- ---------------------------------------------------------------
          0.0  80,526.3    76,163.6 72,338.6 13,058.6 sbt console
          0.0 536,660.4    76,163.6 72,338.6 13,058.6 import java.io._, argonaut._, Argonaut._
          0.0 552,389.1    76,163.6 72,338.6 13,058.6 val file = new File("/Users/jky/Downloads/78mbs.json"
          0.0 634,066.5    76,163.6 72,338.6 13,058.6 val array = new Array[Byte](file.length.asInstanceOf[Int])
          0.0 644,552.3    76,163.6 72,338.6 13,058.6 val is = new FileInputStream("/Users/jky/Downloads/78mbs.json")
          0.0 655,038.1    76,163.6 72,338.6 13,058.6 is.read(array)
    294,976.0 160,159.7 1,100,365.0 79,310.8 13,748.1 val json = new String(array)
    285,182.9 146,392.6 1,956,264.5 82,679.8 14,099.6 val data = Parse.parse(json)
                        ***********

### Parsing large Json files in Haskell with Aeson

    Mem (MB) CMD
    -------- ---------------------------------------------------------
         302 import Data.Aeson
         302 import qualified  Data.ByteString.Lazy as BSL
         302 json78m <- BSL.readFile "/Users/jky/Downloads/78mbs.json"
        1400 let !x = decode json78m :: Maybe Value

### Parsing large Json files in Haskell with hw-succinct

    Mem (MB) CMD
    -------- ---------------------------------------------------------
         274 import Foreign
         274 import qualified Data.Vector.Storable as DVS
         274 import qualified Data.ByteString as BS
         274 import System.IO.MMap
         274 import Data.Word
         274 (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr "/Users/jky/Downloads/78mbs.json" ReadOnly Nothing
         601 cursor <- measure (fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))

## Examples

    import Foreign
    import qualified Data.Vector.Storable as DVS
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Internal as BSI
    import System.IO.MMap
    import Data.Word
    import System.CPUTime
    (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr "/Users/jky/Downloads/78mbs.json" ReadOnly Nothing
    cursor <- measure (fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
    let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
    x <- measure $ jsonBsToInterestBs bs
    let !y = runListConduit [bs] (unescape' "")

    import Foreign
    import qualified Data.Vector.Storable as DVS
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Internal as BSI
    import System.IO.MMap
    import Data.Word
    import System.CPUTime
    (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr "/Users/jky/Downloads/part40.json" ReadOnly Nothing
    let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
    x <- measure $ BS.concat $ runListConduit [bs] (blankJson =$= blankedJsonToInterestBits)
    x <- measure $ jsonBsToInterestBs bs

    jsonTokenAt $ J.nextSibling $ J.firstChild $ J.nextSibling $ J.firstChild $ J.firstChild  cursor

## References
* [Succinct Data Structures talk by Edward Kmett](https://www.youtube.com/watch?v=uA0Z7_4J7u8)
* [Typed Tagless Final Interpreters](http://okmij.org/ftp/tagless-final/course/lecture.pdf)
* [Conduit Overview](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/conduit-overview)


## Special mentions
* [Sydney Paper Club](http://www.meetup.com/Sydney-Paper-Club/)
