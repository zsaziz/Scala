import scala.util.matching.Regex

object Regexes extends hw.regex.RegexLike {

	def notAlphanumeric : Regex = """[^ a-zA-Z0-9]*""".r

	def time : Regex = """(0\d:[0-5]\d)|(1\d:[0-5]\d)|(2[0-3]:[0-5]\d)""".r

	def phone : Regex = """\((\d){3}\) \d{3}-\d{4}""".r

	def zip : Regex = """\d{5}|(\d{5}-\d{4})""".r

	def comment : Regex =  """/\*.*\*/""".r

	def numberPhrase : Regex = """(twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)(-(one|two|three|four|five|six|seven|eight|nine))?""".r

	def roman : Regex = """(I(I|II|V|X)?)|(V(I|II|III)?)|X(X|XX)?((I(I|II|V|X)?)?|((V(I|II|III)?)?))""".r

	def date : Regex = """((((18|19|20)(04|08|[2468][048]|[13579][26]))|2000)-(02)-29)|((18|19|20)[0-9]{2}-(02)-(0[1-9]|1[0-9]|2[0-8]))|((18|19|20)[0-9]{2}-(0[469]|11)-(0[1-9]|[12][0-9]|30))|((18|19|20)[0-9]{2}-(0[13578]|1[02])-(0[1-9]|[12][0-9]|3[01]))""".r

	def evenParity : Regex = """(([02468]*[13579]){2})*[02468]*""".r
}