import hw.generics._

sealed trait BinTree[A] extends ListLike[A, BinTree[A]]

case class Node[A](lhs: BinTree[A], value: A, rhs: BinTree[A]) extends BinTree[A]{
	def cons(head: A): BinTree[A] = new Node(Leaf(), head, this)

	def head(): Option[A] = {
		headRec(this, value)
	}

	def headRec(atree: BinTree[A], prev: A): Option[A] = atree match{
		case Leaf() => Some(prev)
		case Node(lhs, value, _) => headRec(lhs, value)
	}

	def isEmpty(): Boolean = false

	def tail(): Option[BinTree[A]] = {
		Some(Leaf())
	}

	def tailRec(atree: BinTree[A], prev: BinTree[A], prevV: A): Option[BinTree[A]] = atree match{
		case Leaf() => Some(new Node(Leaf(), prevV, prev))
		case Node(Leaf(), x, y) => Some(new Node(Leaf(), x, prev))
		case Node(lhs, value, rhs) => tailRec(lhs, atree, value)
	}
}

case class Leaf[A]() extends BinTree[A]{
	def cons(head: A): BinTree[A] = new Node(Leaf(), head, Leaf())

	def head(): Option[A] = None

	def isEmpty(): Boolean = true

	def tail(): Option[BinTree[A]] = None
}

object ListFunctions{
	def filter[A](f: A => Boolean, alist: ListLike[A, BinTree[A]]): ListLike[A, BinTree[A]] = alist match{
		case Leaf() => new Leaf()
		case Node(lhs, value, rhs) => {
			if(f(value)) new Node(Leaf(), value, Leaf())
			else append(filter(f, lhs), filter(f, rhs))
		}
	}

	def append[A](alist1: ListLike[A, BinTree[A]], alist2: ListLike[A, BinTree[A]]): ListLike[A, BinTree[A]] = (alist1, alist2) match{
		case (Leaf(), Leaf()) => Leaf()
		case (Leaf(), Node(lhs, value, rhs)) => alist2
		case (Node(lhs, value, rhs), Leaf()) => alist1
		case (Node(lhs1, value1, rhs1), Node(lhs, value, rhs)) => (alist1.head(), alist1.tail()) match {
			case (None, None) => alist2
			case (None, Some(y)) => alist2
			case (Some(x), Some(y)) => append(y, alist2.cons(x))
			case (Some(x), None) => alist2.cons(x)
		}
	}

	def sort[A <: Ordered[A], C <: ListLike[A, C]](alist: C): C = alist
}



class C1 extends T2[Int, Int, String, String] with T3[Int, Int, Int, String, String, String, Int]{
	def f(a: Int ,b: Int): Int = 0
	def g(c: String): String = ""
	def h(d: String): Int = 0
}

class C2 extends T1[Int, Int] with T2[Int, Int, Int, Int] with T3[Int, Int, Int, Int, Int, Int, Int]{
	def f(a: Int, b: Int ): Int = 0
	def g(c: Int ): Int = 0
	def h(d: Int ): Int = 0
}

class C3 [A](x: A) extends T3[Int, A, Int, A, String, String, A]{
	def f(a: Int, b: A): Int = 0
	def g(c: A ): String = ""
	def h(d: String): A = x
}

class C4 [A](x: Int, y: C4[A]) extends T3[Int, C4[A], C4[A], Int, C4[A], C4[A], Int]{
	def f(a: Int, b: C4[A]): C4[A] = b
	def g(c: Int): C4[A] = y
	def h(d: C4[A]): Int = x
}