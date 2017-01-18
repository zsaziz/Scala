import FunctionalDataStructures._

class TestSuite extends org.scalatest.FunSuite{

	def fromList[A](alist: List [A]): JoinList [A] = alist match {
	case Nil => Empty ()
	case List (x) => Singleton (x)
	case _ => {
		val len = alist.length
		val (lhs, rhs) = alist.splitAt(len/2)
		Join(fromList(lhs), fromList(rhs), len)
		}
	}

	def toList[A](alist: JoinList [A]): List[A] = alist match {
		case Empty () => Nil
		case Singleton(x) => List(x)
		case Join (alist1, alist2, _) => toList(alist1) ++ toList(alist2)
	}

	val x = List(1, 2, 3, 4)
	val y = fromList(x)

	test("first"){
		assert(first(y) == Some(1))
	}

	test("max"){
		def f(p: Int, q: Int): Boolean = p > q
		assert(max(y, f) == Some(4))
	}

	test("map"){
		def f(p: Int): Int = p + 1
		assert(map(f, y) == fromList(List(2,3,4,5)))
	}

	test("nth"){
		assert(nth(y, 2) == Some(3))
	}

	test("rest"){
		assert(rest(y) == Some(fromList(List(2,3,4))))
	}

	test("filter"){
		def f(p: Int): Boolean = p % 2 == 0
		assert(filter(f, fromList(List(2))) == fromList(List(2)))
	}
}