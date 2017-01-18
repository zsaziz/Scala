import java.nio.file._
import java.time.LocalDate

object PathImplicits{

	implicit class RichString(path: String){
		def /(s: String): Path = Paths.get(path + "/" + s)
	}

	implicit class RichPath(path: Path) {

		def /(p: Path): Path = Paths.get(path.toString + "/" + p.toString)

		def write(s: String): Path = {
			if (Files.exists(path)){
				Files.write(path, s.getBytes())
			}
			else{
				Files.write(Files.createFile(path), s.getBytes())
			}
		}

		def read(): String = {
			readHelp("", Files.readAllLines(path), 0)
		}

		private def readHelp(s: String, alist: java.util.List[String], count: Int): String = {
			if (alist.isEmpty())
				""
			else{
				s + alist.get(count) + "\n" + readHelp(s, alist.subList(1, alist.size()) ,count)
			}
		}

		def append(s: String): Path = {
			if (!Files.exists(path))
				Files.createFile(path)
			Files.write(path, s.getBytes(), java.nio.file.StandardOpenOption.APPEND)
		}
	}
}

object DateImplicits{

	implicit class RichDate(x: Int) {

		def jan(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 1, x)

		def feb(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 2, x)

		def mar(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 3, x)

		def apr(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 4, x)

		def may(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 5, x)

		def jun(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 6, x)

		def jul(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 7, x)

		def aug(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 8, x)

		def sep(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 9, x)

		def oct(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 10, x)

		def nov(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 11, x)

		def dec(): LocalDate = LocalDate.of(LocalDate.now().getYear(), 12, x)

		def jan(year: Int): LocalDate = LocalDate.of(year, 1, x)

		def feb(year: Int): LocalDate = LocalDate.of(year, 2, x)

		def mar(year: Int): LocalDate = LocalDate.of(year, 3, x)

		def apr(year: Int): LocalDate = LocalDate.of(year, 4, x)

		def may(year: Int): LocalDate = LocalDate.of(year, 5, x)

		def jun(year: Int): LocalDate = LocalDate.of(year, 6, x)

		def jul(year: Int): LocalDate = LocalDate.of(year, 7, x)

		def aug(year: Int): LocalDate = LocalDate.of(year, 8, x)

		def sep(year: Int): LocalDate = LocalDate.of(year, 9, x)

		def oct(year: Int): LocalDate = LocalDate.of(year, 10, x)

		def nov(year: Int): LocalDate = LocalDate.of(year, 11, x)

		def dec(year: Int): LocalDate = LocalDate.of(year, 12, x)

		def days(): ((Int, String)) = ((x, "days"))

		def months(): ((Int, String)) = ((x, "months"))

		def years(): ((Int, String)) = ((x, "years"))
	}

	implicit class RDate(date: LocalDate){
		def +(tuple: ((Int, String))): LocalDate = {
			if (tuple._2 == "days")
				date.plusDays(tuple._1)
			else if (tuple._2 == "months")
				date.plusMonths(tuple._1)
			else
				date.plusYears(tuple._1)
		}
	}
}