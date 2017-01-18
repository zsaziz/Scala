import PathImplicits._
import DateImplicits._

class TestSuite extends org.scalatest.FunSuite{

	val a = java.nio.file.Paths.get("test.txt")

	test("write"){
		assert(a.write("Hello") == java.nio.file.Paths.get("test.txt"))
		assert(a.read() == "Hello")
		assert(a.append("\nZain").read() == "Hello\nZain")
		assert(a.append("\nI am amazing!").read() == "Hello\nZain\nI am amazing!")
	}
}