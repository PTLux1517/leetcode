package leetcode.easy

import leetcode.{ColorPrinter, LeetcodeProblem}

import java.time.{Duration, Instant, temporal}, temporal.Temporal

import scala.concurrent.duration.FiniteDuration


object P0_FizzBuzz extends LeetcodeProblem {

  val prob:Array[String] = this.getClass.getSimpleName.split('_')
  val probNum:String = prob.head.drop(1) //drop leading P
  val probName:String = prob.tail.mkString(" ").dropRight(1) //drop trailing $



//  def intToFB: Int => String
//             = i   => {
//    (i%3,i%5) match {
//      case (0,0) => "FizzBuzz"
//      case (0,_) => "Fizz"
//      case (_,0) => "Buzz"
//      case _     => i.toString
//    }
//  }
//
//  def fizzbuzz: Int => Unit
//              = n   => {
//    println((1 to n).map(intToFB).mkString("[",", ","]"))
//  }



//  def f: Int    => (Int => String) => Int  => Option[String]
//       = modulo => replacer        => elem => {
//    if elem % modulo == 0 then Some(replacer(elem)) else None
//  }
//
//  def fizzbuzz: Int => String
//              = {
//    f(15)(_ => "FizzBuzz").unlift orElse
//    f(3)(_ => "Fizz").unlift orElse
//    f(5)(_ => "Buzz").unlift orElse
//    f(1)(_.toString).unlift
//  }



//  def f: Int    => (Int => String) => PartialFunction[Int,String]
//       = modulo => replacer        => {
//    case elem if elem % modulo == 0 => replacer(elem)
//  }
//
//  def fizzbuzz: PartialFunction[Int,String]
//              = {
//    f(15)(_=>"FizzBuzz") orElse
//    f(3)(_=>"Fizz") orElse
//    f(5)(_=>"Buzz") orElse
//    f(1)(_.toString)
//  }



  private val replacementMap = Map(
    3 -> "Fizz",
    5 -> "Buzz",
    7 -> "Pop"
  )

  def fb: Map[Int,String] => Iterable[Int] => Iterable[String]
        = replMap         => input         => {
    input.map(elem => {
      var str = ""
      replMap.foreach((k,v) => {if (elem % k == 0) str += v})
      if str.isEmpty then elem.toString else str
    })
  }



  def run():FiniteDuration = {
    /* Provided input */
    val arg1 = 105

    /* Problem description with computed output */
    print(f"\nProblem ${probNum}: ${probName}\n")

    /* Computed output with run time */
    val start = Instant.now()
//    fizzbuzz(arg1)
//    (1 to arg1).foreach(fizzbuzz andThen println)
//    print((1 to arg1).map(fizzbuzz).mkString("[",", ","]"))
    print(fb(replacementMap)(1 to arg1).mkString("[",", ","]"))
    val end = Instant.now()

    /* Pass/Fail status */
    ColorPrinter.printPassFail(pass = true)

    Duration.between(start,end)
  }
}
