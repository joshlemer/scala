package scala.collection.immutable

package scala.collection.mutable

import org.scalacheck._
import org.scalacheck.Prop.forAll

import Gen._

object ImmutableBitSetProperties extends Properties("immutable.BitSet") {

  // the top of the range shouldn't be too high, else we may not get enough overlap
  implicit val arbitraryBitSet: Arbitrary[BitSet] =
    Arbitrary(listOfN(200, oneOf(0 to 10000)).map(_.to(BitSet)))

  property("removeAll") = forAll { (left: BitSet, right: BitSet) =>

    if (left.removeAll(right) != left.to(HashSet).removeAll(right.to(HashSet))) {
      val a = left.removeAll(right)
      val b = left.to(HashSet).removeAll(right.to(HashSet))
        false
    } else true
  }
}

