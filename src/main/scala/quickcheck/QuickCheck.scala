package quickcheck

import common._
import org.scalacheck._

import scala.collection.immutable.IntMap

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

	object BinomialIntHeap extends BinomialHeap with IntHeap

	lazy val genHeap: Gen[H] = for {
		v <- Arbitrary.arbitrary[A]
		m <- Gen.oneOf(Gen.const(empty), genHeap)
	} yield insert(v, m)

	implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

	property("gen1") = Prop.forAll { (h: H) =>
		val m = if (isEmpty(h)) 0 else findMin(h)
		findMin(insert(m, h)) == m
	}

	property("sorted") = Prop.forAll { (h: H) =>
		def isSorted(last: Int, heap: H): Boolean = {
			isEmpty(heap) || {
				val min = findMin(heap)
				last <= min && isSorted(min, deleteMin(heap))
			}
		}

		isEmpty(h) || isSorted(findMin(h), deleteMin(h))
	}

	property("minOfSome") = Prop.forAll { (h1: H, h2: H) =>
		val m1 = if(isEmpty(h1)) 0 else findMin(h1)
		val m2 = if(isEmpty(h2)) 0 else findMin(h2)
		val m = findMin(meld(h1, h2))

		m == m1 || m == m2
	}
}
