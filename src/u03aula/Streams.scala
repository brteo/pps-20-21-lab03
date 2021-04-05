package u03aula

import scala.util.Random

object Streams extends App {

  import u03.Lists._

  sealed trait Stream[A]

  object Stream {

    private case class Empty[A]() extends Stream[A]

    private case class Cons[A](head: () => A, tail: () => Stream[A]) extends Stream[A]

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def toList[A](stream: Stream[A]): List[A] = stream match {
      case Cons(fh,ft) => List.Cons(fh(),toList(ft()))
      case Empty() => List.Nil()
    }

    def map[A, B](stream: Stream[A])(mapper: A => B): Stream[B] = stream match {
      case Cons(fh,ft) => cons(mapper(fh()),map(ft())(mapper))
      case Empty() => empty()
    }

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match {
      case Cons(fh,ft) if (pred(fh()))=> cons(fh(),takeWhile(ft())(pred))
      case _ => empty()
    }

    def peek[A](stream: Stream[A])(exec: A => Unit): Stream[A] = map(stream)(x => {exec(x);x})

    def fold[A, B](stream: Stream[A])(base: => B)(op: (A, B) => B): B = stream match {
      case Cons(fh,ft) => op(fh(),fold(ft())(base)(op))
      case Empty() => base
    }

    def iterate[A](seed: => A)(next: A => A): Stream[A] = cons(seed,iterate(next(seed))(next))
    def generate[A](elem: => A): Stream[A] = iterate(elem)(x=>x)
  }

  import Stream._
  var s = cons(10,cons(11, cons(20,empty())))
  println(s)
  println(toList(s))
  println(toList(map(s)(_+1))) // [11,12,21]
  println(toList(takeWhile(s)(_<15))) // [11,12]
  println(toList(peek(s)(println(_))))
  println(fold(s)(0)(_+_))
  println(toList(takeWhile(iterate(0)(_+1))(_<100)))
  println(toList(takeWhile(generate(Math.random()))(_>0.1)))
}

