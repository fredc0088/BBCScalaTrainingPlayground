package Lab03

/**
  * 1. Define a simple Algebraic Sum Data Type to represent days of the week. Provide the methods tomorrow and nextBusinessDay for the type.
  * 2. Look at the Box[A] type defined in the slides. What changes, if any, are needed to make this type covariant in its element type? Make the same changes to the MyList[A] type.
  * 3. Using the fold method defined on the MyList[A] type, implement the method
  *               contains(el: A): Boolean
  * which returns true if the list contains the element supplied as the parameter, and false otherwise.
  * 4. Binary Tree is a type that can easily be defined using a recursive ADT. A Binary Tree is
  *   • A leaf node, which contains a value of some type
  *   • An internal node, which contains a left and a right node
  * Implement this type, and provide a fold method for it. Using the fold method, implement a length method, which returns the number of leaf nodes in the tree.
  */
object Main extends App {

  "--------------------1----------------------"

  sealed trait Day {
    def tomorrow: Day = {
      this match {
        case Monday => Tuesday
        case Tuesday => Wednesday
        case Wednesday => Thursday
        case Thursday => Friday
        case Friday => Saturday
        case Saturday => Sunday
        case Sunday => Monday
      }
    }

    private def isBusinessDay =
      Seq(Saturday, Friday) contains this

    def nextBusinessDay =
      if(isBusinessDay) Monday
      else this.tomorrow
  }

  case object Monday extends Day
  case object Tuesday extends Day
  case object Wednesday extends Day
  case object Thursday extends Day
  case object Friday extends Day
  case object Saturday extends Day
  case object Sunday extends Day


  "--------------------2-3----------------------"

  sealed trait MyList[+A] {
    def fold[B >: A](end: B, f: (A,B) => B): B =
      this match {
        case End() => end
        case Cons(hd, tl) => f(hd, tl.fold(end, f))
      }

    def length: Int = fold[Int](0, (_, b) => 1 + b)

    def contains[B >: A](el: B): Boolean =
      this match {
        case End() => false
        case Cons(h, End()) => h == el
        case _ => fold[Boolean](false, (x , b) => if(x == b) return true else b)
      }
  }

  case class Cons[+A](hd: A, tl: MyList[A]) extends MyList[A]
  case class End[+A]() extends MyList[A]


  "--------------------4----------------------"

  sealed trait Tree[A] {

    def fold[B](accumulator: (B, B) => B, f: A => B): B = {
      this match {
        case Leaf(v) => f(v)
        case Branch(l,r) => accumulator(f(l), f(r))
      }
    }

//    def contains[B](el: B) =
//      fold()

    def length = fold[Int]((a: Int, b: Int) => a + b, _ => 1)
  }
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
}
