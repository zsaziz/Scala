import scala.util.matching.Regex
import Regexes._

class TrivialTestSuite extends org.scalatest.FunSuite{
	
	test("The Regexes object must be defined"){
		val regexes: hw.regex.RegexLike = Regexes
	}

	test("notAlphanumeric"){
		assert(notAlphanumeric.pattern.matcher("a").matches() == false)
		assert(notAlphanumeric.pattern.matcher("1").matches() == false)
		assert(notAlphanumeric.pattern.matcher("!").matches() == true)
		assert(notAlphanumeric.pattern.matcher("!1").matches() == false)
		assert(notAlphanumeric.pattern.matcher("!a").matches() == false)
		assert(notAlphanumeric.pattern.matcher("a!").matches() == false)
	}

	test("time"){
		val a = time
		assert(a.pattern.matcher("12:34").matches() == true)
		assert(a.pattern.matcher("00:00").matches() == true)
		assert(a.pattern.matcher("23:59").matches() == true)
		assert(a.pattern.matcher("00:60").matches() == false)
		assert(a.pattern.matcher("24:00").matches() == false)
	}

	test("phone"){
		val a = phone
		assert(a.pattern.matcher("(617) 982-9469").matches() == true)
		assert(a.pattern.matcher("(617)982-9469").matches() == false)
		assert(a.pattern.matcher("617 982-9469").matches() == false)
		assert(a.pattern.matcher("(617) 982 9469").matches() == false)
		assert(a.pattern.matcher("617982-9469").matches() == false)
	}

	test("zip"){
		val a = zip
		assert(a.pattern.matcher("02141").matches() == true)
		assert(a.pattern.matcher("02141-1234").matches() == true)
		assert(a.pattern.matcher("02141-12345").matches() == false)
		assert(a.pattern.matcher("02141-123").matches() == false)
		assert(a.pattern.matcher("0211-1234").matches() == false)
		assert(a.pattern.matcher("0214").matches() == false)
	}

	test("comment"){
		val a = comment
		assert(a.pattern.matcher("/**/").matches() == true)
		assert(a.pattern.matcher("/*02141-1234*/").matches() == true)
		assert(a.pattern.matcher("a/*a*/").matches() == false)
	}

	test("numberPhrase"){
		val a = numberPhrase
		assert(a.pattern.matcher("fifty").matches() == true)
		assert(a.pattern.matcher("twenty-one").matches() == true)
		assert(a.pattern.matcher("ninety-one").matches() == true)
		assert(a.pattern.matcher("seventy-five").matches() == true)
		assert(a.pattern.matcher("twentyfive").matches() == false)
		assert(a.pattern.matcher("five-eighty").matches() == false)
	}

	test("roman"){
		val a = roman
		assert(a.pattern.matcher("I").matches() == true)
		assert(a.pattern.matcher("IV").matches() == true)
		assert(a.pattern.matcher("IX").matches() == true)
		assert(a.pattern.matcher("XI").matches() == true)
		assert(a.pattern.matcher("XXVII").matches() == true)
		assert(a.pattern.matcher("XXVIII").matches() == true)
		assert(a.pattern.matcher("XXXV").matches() == true)
		assert(a.pattern.matcher("VXI").matches() == false)
	}

	test("date"){
		val a = date
		assert(a.pattern.matcher("2016-01-31").matches() == true)
		assert(a.pattern.matcher("2016-02-29").matches() == true)
		assert(a.pattern.matcher("2017-02-29").matches() == false)
		assert(a.pattern.matcher("1999-06-30").matches() == true)
		assert(a.pattern.matcher("2016-07-31").matches() == true)
		assert(a.pattern.matcher("2016-08-31").matches() == true)
	}

	test("evenParity"){
		val a = evenParity
		assert(a.pattern.matcher("203").matches() == false)
		assert(a.pattern.matcher("204").matches() == true)
		assert(a.pattern.matcher("1").matches() == false)
		assert(a.pattern.matcher("0000000002020202020202").matches() == true)
		assert(a.pattern.matcher("0").matches() == true)
	}

}