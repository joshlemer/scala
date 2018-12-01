package scala.collection.mutable

import org.scalacheck._
import org.scalacheck.Prop.forAll

import Gen._

object MutableBitSetProperties extends Properties("mutable.BitSet") {

  // the top of the range shouldn't be too high, else we may not get enough overlap
  implicit val arbitraryBitSet: Arbitrary[BitSet] =
    Arbitrary(listOf(oneOf(0 to 100)).map(_.to(BitSet)))

  property("&~=") = forAll { (left: BitSet, right: BitSet) =>
    val leftClone = left.clone()
    leftClone.filterNot(right.contains) == (left &~= right)
  }
  property("subtractAll") = forAll { (left: BitSet, right: BitSet) =>
    val leftClone = left.clone()
    (leftClone &~= right) == (left subtractAll right)
  }

}
