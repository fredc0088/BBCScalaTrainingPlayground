package Lab01a

import Lab01a.lab1a._

/**
  * Based on the example shown in the slides, define a series of classes that represent measures of distance (or length). For example, centimetre, metre, kilometre, etc. Include also imperial measures such as mile, yard, foot and inch. Provide appropriate means of constructing instances of the types, and some supported operations (basic arithmetic, ordering and comparison, etc.)  For the time being, you can restrict operations so that they are all restricted to instances of the same unit type.  Do the same for measures of temperature (Celsius, Fahrenheit, Kelvin).  
  * Implement a series of functions that allow you to convert from one of the unit types to another (e.g. from degrees Fahrenheit to degrees Celsius and back). You will probably find yourself writing a lot of boilerplate code, so you can choose to restrict your examples to a subset of the types.  
  */
object TrainerSolution {

  abstract class Length(value: Double) extends Ordered[Length] {
    val conversion: Double
    private[Lab01a] def metre = Metre(value / conversion)
    def compare(that: Length): Int = metre.compare(that.metre)
    def toCentimetres: Centimetre = Centimetre(value / conversion)
    def toMetres: Metre = Metre(value / conversion)
    def toKilometres: Kilometre = Kilometre(value / conversion)
  }

  case class Metre(value: Double) extends Length(value: Double) {
    override val conversion = 1
    def +(other: Length): Metre = Metre(value + other.metre.value)
    def compare(that: Metre): Int = value.compare(that.value)
  }

  case class Centimetre(value: Double) extends Length(value: Double) {
    override val conversion: Double = 100
    def this(m: Metre) {this(m.value * 100)}
    def +(other: Length): Centimetre = new Centimetre(metre + other.metre)
  }

  case class Kilometre(value: Double) extends Length(value: Double) {
    override val conversion = 0.001
    def this(m: Metre) {this(m.value * 0.001)}
    def +(other: Length): Kilometre = new Kilometre(metre + other.metre)
  }

  def main(args: Array[String]): Unit = {
    val cm1 = Centimetre(3)
    val cm2 = Centimetre(2)
    println(cm1 + cm2)
    println(cm1.compare(Kilometre(2)))
    println(Metre(1).compare(cm1))
    println(cm2 + Kilometre(3))

    // use function in package object
    println(convertCentimetersToKilometre(cm2))

    // same function, implemented as a method of the unit class
    println(cm1.toKilometres)
  }


}
