package u03aula

import scala.util.Random

object UseStreams extends App {

  import Streams.Stream._

  val n = Random.nextInt(100)
  var str = generate({print("try: "); scala.io.StdIn.readLine()})
  var str2 = map(str)(Integer.parseInt(_))
  str2 = takeWhile(str2)(_ != n)
  var str3 = map(str2)({ case num if (num > n) => "higher"; case _ => "lower" })
  str3 = peek(str3)(s => println("yours is: " + s))
  println("you won in " + fold(str3)(1)((a, b) => b + 1) + " steps")
}

