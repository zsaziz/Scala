object FunctionalDataStructures{
	case class Queue[A](front: List[A], back: List[A])

	def enqueue[A](elt: A, q: Queue[A]): Queue[A] = q.front match {
		case Nil => Queue[A](elt :: Nil, Nil)
		case _ => Queue[A](q.front, elt :: q.back)
	}

	def dequeue[A](q: Queue[A]): Option[(A, Queue[A])] = (q.front, q.back) match {
		case (Nil, Nil) => None
		case (head :: tail, _) => Some(head, Queue[A](tail, q.back))
		case (Nil, lst) => Some(lst.reverse.head, Queue[A](lst.reverse.tail,Nil))
	}

	sealed trait JoinList[A]{
		val size: Int
	}

	case class Empty[A]() extends JoinList[A]{
		val size = 0
	}

	case class Singleton[A](elt: A) extends JoinList[A]{
		val size = 1
	}

	case class Join[A](lst1: JoinList[A], lst2: JoinList[A], size: Int) extends JoinList[A]

	def max[A](lst: JoinList[A], compare: (A, A) => Boolean): Option[A] = lst match{
		case Empty() => None
		case Singleton(x) => Some(x)
		case Join(lst1, lst2, _) => {
			val max1 = max(lst1, compare)
			val max2 = max(lst2, compare)
			(max1, max2) match{
				case (Some(x), Some(y)) => if (compare(x,y)) Some(x); else Some(y)
				case (Some(x), None) => Some(x)
				case (None, Some(y)) => Some(y)
				case (None, None) => None
			}
		}
	}

	def first[A](lst: JoinList[A]): Option[A] = lst match {
		case Empty() => None
		case Singleton(elt) => Some(elt)
		case Join(Empty(), lst2, _) => first(lst2)
		case Join(lst1, lst2, _) => first(lst1)
	}

	def rest[A](lst: JoinList[A]): Option[JoinList[A]] = lst match {
		case Empty() => None
		case Singleton(elt) => Some(Empty())
		case Join(left, right, _) => (rest(left), rest(right)) match {
			case (Some(lst1), Some(lst2)) => Some(Join(lst1, lst2, lst1.size + lst2.size))
			case (Some(lst1), None) => Some(Join(lst1, Empty(), lst1.size))
			case (None, Some(lst2)) => Some(Join(Empty(), lst2, lst2.size))
			case (None, None) => Some(Join(Empty(), Empty(), 0))
		}
		
	}

	def map[A, B](f: A => B, lst: JoinList[A]): JoinList[B] = lst match {
		case Empty() => Empty()
		case Singleton(elt) => Singleton(f(elt))
		case Join(lst1, lst2, _) => Join(map(f, lst1), map(f, lst2), lst1.size+lst2.size)
	}

	def nth[A](lst: JoinList[A], n: Int): Option[A] = lst match {
		case Empty() => None
		case Singleton(elt) => if (n == 0) Some(elt); else None
		case Join(left, right, _) => {
			if (n < left.size) nth(left, n)
			else nth(right, n - left.size)
		}
	}

	def filter[A](prod: A => Boolean, lst: JoinList[A]): JoinList[A] = lst match {
		case Empty() => Empty()
		case Singleton(elt) => if (prod(elt)) Singleton(elt); else Empty()
		case Join(lst1, lst2, _) => {
			val left = filter(prod, lst1); val right = filter(prod, lst2)
			Join(left, right, left.size+right.size)
		}
	}
}