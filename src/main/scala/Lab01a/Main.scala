package Lab01a

import Lab01a.MyDistance.{Centimetre, Foot, Kilometre, Mile}
import Lab01a.MyTemperature._

/**
  * Based on the example shown in the slides, define a series of classes that represent measures of distance (or length). For example, centimetre, metre, kilometre, etc. Include also imperial measures such as mile, yard, foot and inch. Provide appropriate means of constructing instances of the types, and some supported operations (basic arithmetic, ordering and comparison, etc.)  For the time being, you can restrict operations so that they are all restricted to instances of the same unit type.  Do the same for measures of temperature (Celsius, Fahrenheit, Kelvin).  
  * Implement a series of functions that allow you to convert from one of the unit types to another (e.g. from degrees Fahrenheit to degrees Celsius and back). You will probably find yourself writing a lot of boilerplate code, so you can choose to restrict your examples to a subset of the types.  
  */
object Main extends App {

  assert((Kilometre(20) - Kilometre(10)).v == 10)
  println(Kilometre(30.0) - Centimetre(2.0) - Mile(1) + Foot(10000))

}