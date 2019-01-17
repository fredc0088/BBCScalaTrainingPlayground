package Lab02b

/**
  * Define a typeclass to represent “Ordering” of another type. The typeclass should provide two methods, < and >, which have the expected meaning. You can leverage other types, such as java.lang.Comparable[T] to help in your definition.
  * Create instances of your typeclass for simple numeric types such as Int.
  * Define a class to represent Rational Numbers (Fractions). Provide an instance of your typeclass to for this class.
  * Experiment with using Context Bounds in defining other methods that require their argument type to implement the functionality provided by the typeclass.
  */
object Main2 {

  trait Ordering[A] extends Comparable[A]{

    val value: A

    def >(o: A) =
      compareTo(o) > 0

    def <(o: A) =
      compareTo(o) < 0
  }

  case class Fraction(numerator: Int, denominator: Int)

  implicit val int5Ordering = new Ordering[Int]{
    override val value: Int = 5
    override def compareTo(o: Int): Int =
      if(o > value) -1
      else if(o == value) 0
      else 1
  }

  implicit val `rational3/2Ordering` = new Ordering[Fraction] {
    override val value: Fraction = Fraction(3,2)
    override def compareTo(o: Fraction): Int = {
      val (a, b) = (value.numerator/value.denominator,o.numerator/o.denominator)
      if(a > b) 1
      else if(a == b) 0
      else -1
    }
  }

  implicit def lessThan(i: Int) =  (implicitly[Ordering[Int]] > i, "s")

  def main(args: Array[String]): Unit = {
    println(6._1)
  }


}
