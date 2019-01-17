package Lab04

trait Monoid[A] {
  def op(f1: A, f2: A): A
  def zero: A
}