package exercise

import u03.Lists.List._
import u03.Streams._
import exercise.Streams._

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class StreamsTest {

  // 5
  @Test def dropTest(){
    val s = Stream.take(Stream.iterate(0)(_+1))(10)

    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(drop(s)(6)))
  }

  // 6
  @Test def constantTest(){
    val x1=1
    assertEquals(Cons(x1,Cons(x1,Cons(x1,Cons(x1,Cons(x1,Nil()))))), Stream.toList(Stream.take(constant(x1))(5)))

    val x2="Test"
    assertEquals(Cons(x2,Cons(x2,Cons(x2,Cons(x2,Cons(x2,Nil()))))), Stream.toList(Stream.take(constant(x2))(5)))
  }

  // 7
  @Test def fibTest(){
    assertEquals(Cons(0,Cons(1,Cons(1,Cons(2,Cons(3,Cons(5,Cons(8,Cons(13,Nil())))))))), Stream.toList(Stream.take(fibs)(8)))
  }

}