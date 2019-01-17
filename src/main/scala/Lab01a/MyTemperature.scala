package Lab01a

import scala.reflect.ClassTag

object MyTemperature {
    sealed trait Temperature {
      type T = this.type
      val v: Double

      protected def toCelsius: Double

      def convertBack(value: Double): Double

      private object DoubleBox {
        implicit def box(d: Double) = DoubleBox(d)
      }
      private case class DoubleBox(private val v: Double) {
        def unboxVal = v.asInstanceOf[java.lang.Double]
      }
      import DoubleBox._

      private def wrap(c: ClassTag[T], value: DoubleBox) = {
        val m = c.runtimeClass.getConstructor(classOf[Double])
        m.newInstance(value.unboxVal).asInstanceOf[T]
      }

      def +(other: Temperature)(implicit manifest: ClassTag[T]): T = {
        val result = this.toCelsius + other.toCelsius
        wrap(manifest, convertBack(result))
      }

      def -(other: Temperature)(implicit manifest: ClassTag[T]): T = {
        val result = this.toCelsius - other.toCelsius
        wrap(manifest, convertBack(result))
      }

      def ==(other: Temperature): Boolean =
        this.toCelsius == other.toCelsius
    }

    case class Celsius(v: Double) extends Temperature {

      override protected def toCelsius: Double = v

      override def convertBack(value: Double): Double = value
    }

    case class Kelvin(v: Double) extends Temperature {

      val KELVIN_CONSTANT = 273.15

      override protected def toCelsius: Double = v - KELVIN_CONSTANT

      override def convertBack(value: Double): Double = value + KELVIN_CONSTANT
    }

    case class Rankine(v: Double) extends Temperature {

      override protected def toCelsius: Double = (v - 491.67) * (5 / 9)

      override def convertBack(value: Double): Double = (value + 273.15) * 1.8
    }

    case class Fahrenheit(v: Double) extends Temperature {
      override protected def toCelsius: Double = (v - 32) / 1.8

      override def convertBack(value: Double): Double = (v * 1.8) + 32
    }
}
