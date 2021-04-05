package exercise

import u03.Lists._
import u03.Lists.List._

import scala.annotation.tailrec

object Lists {

  // 1.a
  @tailrec
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
}