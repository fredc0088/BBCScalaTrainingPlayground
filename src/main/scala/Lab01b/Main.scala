package Lab01b

import scala.collection.generic.{MapFactory, SeqFactory}


/**
  * 1. A MultiSet, sometimes called a Bag, is a collection that allows duplicates, while not imposing any ordering on the elements.  Define a class to represent a MultiSet in Scala. Use whatever existing collection type(s) you need to underpin the collection types. For each element in the collection, you should maintain an “occurrence” count, reflecting on how many times the element has been added.  Make the MultiSet invariant initially.
  * 2. Change your MultiSet to make it covariant in its element types.
  * 3. Investigate the effects on your implementation of the MultiSet if you use an abstract type member rather than a type parameter.  
  */
object Main extends App {

  class MultiSet[+T]private (values: Vector[T]) {

    def get: Vector[T] = values.distinct

    def count: Seq[(T, Int)] =
      values.groupBy(identity).map(x => (x._1, x._2.size)).toSeq

    def add[A >: T](newEl: A) = MultiSet(newEl +: values)
  }
  object MultiSet {
    def apply[T](get: T*): MultiSet[T] = new MultiSet(get.toVector)
  }

//  class Bag[A] extends SeqFactory[A]{
//
//  }

//  class Bag[+T] extends MapFactory[Map[T, Int]] {
//    override def empty[A, B]: Map[A, B] = Map.empty[A,B]
//
//  }

  class Bag2[+A](val map: Map[Any,Int] = Map[Any,Int]() ) {

    def add[B >: A](el: B){
      new Bag2[B](map + (el -> (map.getOrElse(el, 0) + 1)))
    }
    def getCount(el: Any): Int =
      map.getOrElse(el, 0)

  }

  println(MultiSet("svvv","ytryre","qqqq","svvv", "qqqq", "a", 6).count)

  val b: Bag2[Any] = new Bag2[String]
}
