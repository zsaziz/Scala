object Wrangling{

	import edu.umass.cs.CSV
	val ssa = CSV.fromFile("shortened-births.csv")
	val cdc = CSV.fromFile("cdc-life-expectancy.csv")

	def yearIs(data: List[List[String]], n: Int): List[List[String]] =
		data.filter((rec: List[String]) => rec(0).toInt == n)

	def yearGT(data: List[List[String]], bound: Int): List[List[String]] = 
		data.filter((rec: List[String]) => rec(0).toInt > bound)

	def yearLT(data: List[List[String]], bound: Int): List[List[String]] = 
		data.filter((rec: List[String]) => rec(0).toInt < bound)

	def onlyName(data: List[List[String]], name: String): List[List[String]] =
		data.filter((rec: List[String]) => rec(1).toLowerCase.equals(name.toLowerCase))

	// def mostPopular(data: List[List[String]]): (String, Int)

	def count(data: List[List[String]]): Int = data match{
		case Nil => 0
		case head :: tail => head(3).toInt + count(tail)
	}

	def countGirlsandBoys(data: List[List[String]]): (Int, Int) = data match {
		case Nil => (0,0)
		case head :: tail => {
			if (head(2) == "M"){
				(head(3).toInt + countGirlsandBoys(tail)._1, countGirlsandBoys(tail)._2)
			}
			else{
				(countGirlsandBoys(tail)._1, head(3).toInt + countGirlsandBoys(tail)._2)
			}
		}
	}

	// def GenderNeutralNames(data: List[List[String]]): Set[String]

	def expectedAlive(gender: String, birthYear: Int, currentYear: Int): Boolean = {
		if (birthYear < 1930 || birthYear > 2010) false
		else if(birthYear > currentYear) false
		else {
			if (gender == "M"){
				if (currentYear - yearIs(ssa, birthYear).head(0).toInt <= yearIs(cdc, birthYear).head(1).toInt) true
				else false
			}
			else{
				if (currentYear - yearIs(ssa, birthYear).head(0).toInt <= yearIs(cdc, birthYear).head(2).toInt) true
				else false
			}
		}
	}

	def estimatePopulation(data: List[List[String]], year: Int): Int = data match {
		case Nil => 0
		case head :: tail => {
			if (expectedAlive(head(2), head(0).toInt, year)){
				head(3).toInt + estimatePopulation(tail, year)
			}
			else estimatePopulation(tail, year)
		}
	}

}