package exercise

import u03.Lists._
import u03.Lists.List._
import exercise.Lists._

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class ListsTest {

  val lst = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def dropTest(){
    assertEquals(Cons(20, Cons(30, Nil())),drop(lst,1))
    assertEquals(Cons(30, Nil ()),drop(lst,2))
    assertEquals(Nil(),drop(lst,5))
  }

  @Test def flatMapTest(){
    assertEquals(
      Cons(11,Cons(21,Cons(31,Nil()))),
      flatMap(lst)(v => Cons(v+1, Nil()))
    )
    assertEquals(
      Cons(11,Cons(12,Cons(21,Cons(22,Cons(31,Cons(32,Nil())))))),
      flatMap(lst)(v => Cons(v+1, Cons(v+2, Nil())))
    )
  }

  @Test def mapAsFlatMapTest(){
    assertEquals(
      Cons(11,Cons(21,Cons(31,Nil()))),
      mapAsFlatMap(lst)(v => v+1)
    )
  }

  @Test def filterAsFlatMapTest(){
    assertEquals(Cons(20, Cons(30, Nil())), filter(lst)(_ >=20))
    assertEquals(Cons(30, Nil()), filter(lst)(_ >=30))
    assertEquals(Nil(), filter(lst)(_ < 10))

  }


}