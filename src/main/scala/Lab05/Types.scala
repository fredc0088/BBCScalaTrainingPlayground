package Lab05

object Types {

  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
  }

  trait Monad[M[_]] extends Functor[M]{
    def flatMap[A,B](fa: M[A])(f: A => M[B]): M[B]
    def unit[A](a: => A): M[A]

    override def map[A, B](fa: M[A])(f: A => B): M[B] = flatMap(fa)(a => unit(f(a)))
  }
}
