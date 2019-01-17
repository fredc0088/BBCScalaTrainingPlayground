package Lab01a

import scala.reflect.ClassTag

object MyDistance {

  trait Distance {

    type T = this.type

    val v: Double
    val conversion: Double

    protected def toMetre: Double =
      v * conversion

    private def convertBack(value: Double) =
      value / conversion

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

    def +(other: Distance)(implicit manifest: ClassTag[T]): T = {
      val result = this.toMetre + other.toMetre
      wrap(manifest, convertBack(result))
    }

    def -(other: Distance)(implicit manifest: ClassTag[T]): T = {
      val result = this.toMetre - other.toMetre
      wrap(manifest, convertBack(result))
    }

    def ==(other: Distance): Boolean =
      this.toMetre == other.toMetre

    override def toString: String = s"${this.getClass.getSimpleName}s: ${v}"
  }

  case class Centimetre(v: Double) extends Distance {
    val conversion: Double = 0.001
  }

  case class Metre(v: Double) extends Distance {
    val conversion = 1
  }

  case class Kilometre(v: Double) extends Distance {
    val conversion: Double = 1000
  }

  case class Mile(v: Double) extends Distance {
    val conversion = 1609.34
  }

  case class Foot(v: Double) extends Distance {
    val conversion = 0.3048
  }
}
