package primefactors

import scala.annotation.tailrec

/**
  * Allows for generation of prime factors.
  *
  * The prime factors of a positive integer are the prime numbers that divide
  * that integer exactly, the process if finding these is called
  * prime factorization.
  *
  * [[primefactors.PrimeFactors.primeFactorsLinear primeFactorsLinear]] may be
  * used to provide a linear sequence of all prime factors.
  *
  * [[primefactors.PrimeFactors.primeFactors primeFactors]] may be used to
  * provide a sequence of tuples of all prime factors with their correspondent
  * exponent.
  *
  * See the tests or [[primefactors.Main the Main App]] for an example of use.
  */
object PrimeFactors {

  /**
    * calculates the prime factors and returns a linear vector
    *
    * @param n The number to be prime factorized
    *
    * @return For an arbitrary integer n:
    *   n = p,,1,,^e,,1,,^ * p,,2,,^e,,2,,^ * ... * p,,i,,^e,,i,,^
    *
    *   linear Vector means, that each prime factor is repeated by its exponent
    *   so a result may look like `Vector(2,2,2,3,5,5)`
    */
  def primeFactorsLinear(n: Int): Vector[Int] =
    primeFactors(n, primes, Vector.empty[Int])


  /**
    * calculates the prime factors and returns a tupled vector of schema
    * `prime -> exponent`
    *
    * @param n The number to be prime factorized
    *
    * @return For an arbitrary integer n:
    *   n = p,,1,,^e,,1,,^ * p,,2,,^e,,2,,^ * ... * p,,i,,^e,,i,,^
    *
    *   tupled Vector means, that each element is a tuple `(prime, exponent)`
    */
  def primeFactors(n: Int): Vector[(Int, Int)] =
    linear2tupled(primeFactorsLinear(n))

  /**
    * Converts a linear representation of prime factors into its tupled one.
    *
    * Implementation Detail: There is a groupby method, that would work, except
    * that it returns a Map, which looses ordering and ordering here is
    * important.
    *
    * @param linear The linear representation to be converted
    */
  def linear2tupled(linear: Vector[Int]): Vector[(Int, Int)] = {
    @tailrec def linear2map0(linear0: Vector[Int], prev: Option[Int], acc: Vector[(Int, Int)]): Vector[(Int, Int)] =
      if (linear0.isEmpty) acc
      else linear2map0(linear0.tail, Some(linear0.head),
        prev match {
          case Some(p) if (linear0.head == p) => acc.init :+ (linear0.head -> (acc.last._2 + 1))
          case _ => acc :+ (linear0.head -> 1)
        })

    linear2map0(linear, None, Vector.empty[(Int, Int)])
  }

  /**
    * Converts a tupled representation of prime factors into its linear one
    *
    * @param tuple The tupled representation to be converted
    */
  def tupled2linear(tuple: Vector[(Int, Int)]): Vector[Int] =
    tuple flatMap { case (n, c) => List.fill(c)(n) }

  /**
    * A pseudo-prime-generator.  A very bad one actually, can hardly be called a
    * prime-generator.  It generates all positive odd integers, greater or equal
    * than 3, and adds 2 in fromt of it. So it is basically  {2, 3, 5, 7, ...}
    *
    * This does generate also none prime numbers, but every prime will be
    * generated eventually, which is enough for this use case.
    */
  private val primes: Stream[Int] = 2 #:: Stream.from(3, 2)

  /**
    * Does the actual work of calculating the factors
    *
    * basically, the current number `n` will be divided by the next prime
    * number, until it is no longer divisible by this prime.  If the number is
    * greater than one, repeat with next prime number that `n` is divisible by.
    *
    * The term prime number here, refers to the next number generated by
    * `primes', so these will contain non-prime numbers. But the semantics of
    * the Sieve of Eratosthenes are implemented in the previous step, where `n`
    * will be divided by the current lowest prime.  This is equivalent to
    * crossing of all future numbers, that will be divisible by the prime.
    *
    * So, after all, this guarantees, that only real primes will be used for
    * factorization without the need of costly real-prime-generators or
    * isPrime checks.
    */
  @tailrec private def primeFactors(n: Int, ps: Stream[Int], acc: Vector[Int]): Vector[Int] = n match {
    case 0                     => throw new Error("division by zero")
    case 1                     => acc
    case x if x < 0            => primeFactors(-n, ps, Vector(-1))
    case x if x % ps.head == 0 => primeFactors(x / ps.head, ps, acc :+ ps.head)
    case x                     => primeFactors(x, ps dropWhile { x % _ != 0 }, acc)
  }
}