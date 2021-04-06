package exercise

import u03.Streams._
import u03.Streams.Stream._

object Streams {
  // Ho reso pubblici i due sum types "Cons" ed "Empty" su u03/Streams per utilizzarli in questo package

  // 5
  @annotation.tailrec
  def drop[A](stream: Stream[A])(n: Int): Stream[A] = stream match {
    case Cons(_, t) if n>0 => drop(t())(n-1)
    case _ => stream
  }

  // 6
  def constant[A](x: =>A): Stream[A] = cons(x, constant(x))

  // 7
  def fibs: Stream[Int] = {
    def _fibs(p: =>Int, n: =>Int): Stream[Int] = cons(p, _fibs(n, p + n))
    _fibs(0, 1)
  }
}