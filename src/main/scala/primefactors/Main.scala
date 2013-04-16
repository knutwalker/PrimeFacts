package primefactors

object Main extends App {

  import scala.util.Try
  import PrimeFactors._

  args flatMap { arg => Try(arg.toInt).toOption } map {
    number => primeFactors(number).map(_.productIterator.mkString("^"))
  } zip args foreach {
    case (factors, arg) => println(s"$arg = ${factors.mkString(" * ")}")
  }
}
