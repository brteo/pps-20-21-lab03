package exercise

import u03.Lists.List._
import exercise.Lists._

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class ListsTest {

  val lst:Cons[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  // 1.a
  @Test def dropTest(){
    assertEquals(Cons(20, Cons(30, Nil())),drop(lst,1))
    assertEquals(Cons(30, Nil ()),drop(lst,2))
    assertEquals(Nil(),drop(lst,5))
  }

  // 1.b
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

  // 1.c
  @Test def mapAsFlatMapTest(){
    assertEquals(
      Cons(11,Cons(21,Cons(31,Nil()))),
      mapAsFlatMap(lst)(v => v+1)
    )
  }

  // 1.d
  @Test def filterAsFlatMapTest(){
    assertEquals(Cons(20, Cons(30, Nil())), filter(lst)(_ >=20))
    assertEquals(Cons(30, Nil()), filter(lst)(_ >=30))
    assertEquals(Nil(), filter(lst)(_ < 10))

  }

  // 2
  @Test def maxWithOptionalsTest() {
    import u02.Optionals.Option.Some
    import u02.Optionals.Option.None

    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(None(), max(Nil()))

    assertEquals(Some(-1), max(Cons(-1, Cons(-5, Cons(-2, Nil())))))
    assertEquals(Some(0), max(Cons(-10, Cons(0, Nil()))))
  }

  // 3
  @Test def personListTest() {
    import u02.Modules.Person
    import u02.Modules.Person._

    val input:Cons[Person] =
      Cons(Student("Matteo Brocca", 2020),
        Cons( Student("Alan Mancini", 2020),
          Cons( Teacher("Mirko Viroli", "PPS"),
            Cons( Teacher("Alessandro Ricci", "PCD"),
              Nil()))))

    assertEquals(Cons("PPS", Cons("PCD", Nil())),getCourses(input))
  }

  // 4
  @Test def foldTest(): Unit = {
    val lst = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
    val lst2 = Cons(-5,Cons(-2,Cons(4,Cons(7, Nil()))))

    assertEquals(-16, foldLeft(lst)(0)(_-_))
    assertEquals(16, foldLeft(lst)(0)(_+_))
    assertEquals("nnpp", foldLeft(lst2)("")( (acc,v) => { if (v>0) acc+"p" else acc+"n" }))

    assertEquals(0, foldLeft(Nil[Int]())(0)(_-_))

    assertEquals(-8, foldRight(lst)(0)(_-_))
    assertEquals("ppnn", foldRight(lst2)("")( (v,acc) => { if (v>0) acc+"p" else acc+"n" }))
  }
}