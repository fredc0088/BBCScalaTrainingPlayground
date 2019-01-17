package Lab04

/**
  * 1. Set union is a Monoid. Is set intersection a Monoid? If not, why not?.  
  * 2. Define a Monoid that can be used to “combine” Option[T] values. To combine two Some values, assume you can use a function
  *         combineT: (T, T) => T
  * 3. Using the ProductMonoid from the slides, write a function that calculates the average value of a List of Integers. (Hint: You will need to calculate the sum of the elements, and the number of elements.)  
  * 4. Write a Monoid for the combination of two Function objects using composition. Note that for this to work, the functions should return a value that is the same as that of their argument. (Such functions are known as endofunctions).  
  * 5. Study the MapMergeMonoid from the slides. Using this, implement a method that will construct a Bag[T] (from the earlier lab exercise) from an IndexedSeq[T].  
  */
object Main extends App {

  "-------------------1-------------------"

    // Answer: Yes, because it is an associative operation. lso, a zero for it can be an empty Set.

    /* Notes: Associativity is not the same as Commutativity:
     * ex.: 1 + (2 + 3) is associative because example (1 + 2) + 3 and commutative because example (2 + 3) + 1
     * ex.: "a" + ("b" + "c") is associative because example ("a" + "b") + "c", but not commutative because example ("b" + "c") + "a"
     */

  "-------------------2-------------------"

    trait MergeOptionMonoid[T] extends Monoid[Option[T]] {

      val combineT: (T,T) => T

      override def op(f1: Option[T], f2: Option[T]): Option[T] = {
        (f1, f2) match {
          case (Some(a), Some(b)) => Some(combineT(a,b))
          case _ => f1.orElse(f2)
        }
      }

      override def zero: Option[T] = None
    }

    object MergeOptionMonoidInt extends MergeOptionMonoid[Int]{
      val combineT: (Int, Int) => Int = (a,b) => a + b
    }


  "-------------------1 alternative-------"
    class MergeOptionMonoid2[T](implicit combineT : (T,T) => T) extends Monoid[Option[T]] {

      override def op(f1: Option[T], f2: Option[T]): Option[T] = {
        (f1, f2) match {
          case (Some(a), Some(b)) => Some(combineT(a,b))
          case _ => f1.orElse(f2)
        }
      }

      override def zero: Option[T] = None
    }

  "-------------------3-------------------"




}
