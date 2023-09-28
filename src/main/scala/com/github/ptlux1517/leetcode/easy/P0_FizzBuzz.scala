package com.github.ptlux1517.leetcode.easy

import com.github.ptlux1517.leetcode.{ColorPrinter, LeetcodeProblem}

import java.time.{Duration, Instant, temporal}, temporal.Temporal

import scala.concurrent.duration.{FiniteDuration, DurationInt}
import scala.io.StdIn.readLine


object P0_FizzBuzz extends LeetcodeProblem:

  val className:Array[String] = this.getClass.getSimpleName.split('_')
  val probNum:String = className.head.drop(1) //drop leading P
  val probName:String = className.tail.mkString(" ").dropRight(1) //drop trailing $


  def run():FiniteDuration =
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

  end run


  private val replacementMap = Map(
    3 -> "Fizz",
    5 -> "Buzz",
    7 -> "Pop"
  )

  def fb: Map[Int,String] => Iterable[Int] => Iterable[String]
        = replacementMap  => input         =>
    input.map(elem => {
      var str = ""
      replacementMap.foreach((divisor,replacement) => if (elem % divisor == 0) str += replacement)
      if str.isEmpty then elem.toString else str
    })

  end fb



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



/* Code 128 Barcode Encoder */
//  def run():FiniteDuration = {
//    val charToValMap = Map(
//      'Â' -> 0,
//      '!' -> 1,
//      '"' -> 2,
//      '#' -> 3,
//      '$' -> 4,
//      '%' -> 5,
//      '&' -> 6,
//      '\'' -> 7,
//      '(' -> 8,
//      ')' -> 9,
//      '*' -> 10,
//      '+' -> 11,
//      ',' -> 12,
//      '-' -> 13,
//      '.' -> 14,
//      '/' -> 15,
//      '0' -> 16,
//      '1' -> 17,
//      '2' -> 18,
//      '3' -> 19,
//      '4' -> 20,
//      '5' -> 21,
//      '6' -> 22,
//      '7' -> 23,
//      '8' -> 24,
//      '9' -> 25,
//      ':' -> 26,
//      ';' -> 27,
//      '<' -> 28,
//      '=' -> 29,
//      '>' -> 30,
//      '?' -> 31,
//      '@' -> 32,
//      'A' -> 33,
//      'B' -> 34,
//      'C' -> 35,
//      'D' -> 36,
//      'E' -> 37,
//      'F' -> 38,
//      'G' -> 39,
//      'H' -> 40,
//      'I' -> 41,
//      'J' -> 42,
//      'K' -> 43,
//      'L' -> 44,
//      'M' -> 45,
//      'N' -> 46,
//      'O' -> 47,
//      'P' -> 48,
//      'Q' -> 49,
//      'R' -> 50,
//      'S' -> 51,
//      'T' -> 52,
//      'U' -> 53,
//      'V' -> 54,
//      'W' -> 55,
//      'X' -> 56,
//      'Y' -> 57,
//      'Z' -> 58,
//      '[' -> 59,
//      '\\' -> 60,
//      ']' -> 61,
//      '^' -> 62,
//      '_' -> 63,
//      '`' -> 64,
//      'a' -> 65,
//      'b' -> 66,
//      'c' -> 67,
//      'd' -> 68,
//      'e' -> 69,
//      'f' -> 70,
//      'g' -> 71,
//      'h' -> 72,
//      'i' -> 73,
//      'j' -> 74,
//      'k' -> 75,
//      'l' -> 76,
//      'm' -> 77,
//      'n' -> 78,
//      'o' -> 79,
//      'p' -> 80,
//      'q' -> 81,
//      'r' -> 82,
//      's' -> 83,
//      't' -> 84,
//      'u' -> 85,
//      'v' -> 86,
//      'w' -> 87,
//      'x' -> 88,
//      'y' -> 89,
//      'z' -> 90,
//      '{' -> 91,
//      '|' -> 92,
//      '}' -> 93,
//      '~' -> 94,
//      'Ã' -> 95,
//      'Ä' -> 96,
//      'Å' -> 97,
//      'Æ' -> 98,
//      'Ç' -> 99,
//      'È' -> 100,
//      'É' -> 101,
//      'Ê' -> 102,
//      'Ë' -> 103,
//      'Ì' -> 104,
//      'Í' -> 105
//    )
//    val valToCharMap = charToValMap.map(_.swap)
//
////    val t = ('a' to 'z') zip (65 to 90)
////    val t2 = (33 to 95) zip (1 to 63)
////    for
////      (c,i) <- charToValMap.toSeq.sortBy(_._2)
////    do
////      println(f"'$c' -> $i,")
//
//    val codeAStartChar = 'Ë'
//    print("Enter the code 128 data without the start & end characters: ")
//    val dataString = readLine()
//    var sum:Int = charToValMap(codeAStartChar)
//
//    for
//      (c,i) <- dataString.zipWithIndex
//    do
//      println(f"adding $c: $sum + ${i+1}*${charToValMap(c)}")
//      sum += (i+1) * charToValMap(c)
//    val rem = sum%103
//
//    println(f"sum and remainder mod 103 are: $sum, $rem")
//    println(f"checksum char is: ${valToCharMap(rem)}")
//
//    0.seconds
//  }


end P0_FizzBuzz