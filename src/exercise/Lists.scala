package exercise

import u03.Lists._
import u03.Lists.List._

object Lists {

  // 1.a
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(h, t) if n>0 => drop(t, n-1)
    case _ => l
  }

  // 1.b
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Cons(h, t) => append(f(h), flatMap(t)(f))
    case Nil() => Nil()
  }

  // 1.c
  def mapAsFlatMap[A,B](l: List[A])(mapper: A=>B): List[B] = flatMap(l)(v => Cons(mapper(v),Nil()))

  // 1.d
  def filterAsFlatMap[A](l: List[A])(pred: A=>Boolean): List[A] = flatMap(l)(v => pred(v) match {
    case true => Cons(v,Nil())
    case _ => Nil()
  })

  // 2
  import u02.Optionals.Option
  import u02.Optionals.Option.Some
  import u02.Optionals.Option.None

  def max(l: List[Int]): Option[Int] = {
    @annotation.tailrec
    def _max(l: List[Int], current: Int): Int = l match {
      case Cons(h,t) if current==null || h>current => _max(t, h)
      case Cons(_,t) => _max(t, current)
      case _ => current
    }

    l match {
      case Nil() => None()
      case _ => Some(_max(l,Integer.MIN_VALUE))
    }
  }

  // 3
  import u02.Modules.Person
  import u02.Modules.Person._

  def getCourses(l: List[Person]): List[String] = flatMap(l)(p => p match {
    case Teacher(_,c) => Cons(c,Nil())
    case _ => Nil()
  })
}