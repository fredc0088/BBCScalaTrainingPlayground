package Lab02b

/**
  * Define a typeclass to represent “Ordering” of another type. The typeclass should provide two methods, < and >, which have the expected meaning. You can leverage other types, such as java.lang.Comparable[T] to help in your definition.
  * Create instances of your typeclass for simple numeric types such as Int.
  * Define a class to represent Rational Numbers (Fractions). Provide an instance of your typeclass to for this class.
  * Experiment with using Context Bounds in defining other methods that require their argument type to implement the functionality provided by the typeclass.
  */
object TrainerSolution {

  trait Compare[A] {
    def myCompare(v1: A, v2: A): Boolean
  }

  case class Fraction(num: Int, den: Int)

  implicit val fractionOrdering = new Compare[Fraction] {
    override def myCompare(v1: Fraction, v2: Fraction): Boolean =
      v1.num.toDouble/v1.den < v2.num.toDouble/v2.den
  }

  def <[A: Compare](v1: A, v2: A) = implicitly[Compare[A]].myCompare(v1,v2)

  def main(args: Array[String]): Unit = {
    println(<(Fraction(1,2),Fraction(3,4)))
  }


}
