package com.github.ptlux1517.leetcode

import com.github.ptlux1517.leetcode.{ColorPrinter, LeetcodeProblem}

import java.time.{Duration, Instant, temporal}, temporal.Temporal

import scala.concurrent.duration.FiniteDuration


object Pb_Template extends LeetcodeProblem:

  val className:Array[String] = this.getClass.getSimpleName.split('_')
  val probNum:String = className.head.drop(1) //drop leading P
  val probName:String = className.tail.mkString(" ").dropRight(1) //drop trailing $


  def run():FiniteDuration =
    /* Provided input */
    val arg1 = 0

    /* Expected output */
    val exp = 0

    /* Computed output with run time */
    val start = Instant.now()
    val sol:Unit = providedMethod()
    val end = Instant.now()

    /* Problem description with computed output */
    print(f"""
      |Problem $probNum: $probName
      |<Description>
      |
      |Constraints:
      |* _
      |
      |Input: ${arg1}
      |Output: ${sol}
      |Expected: ${exp}
      |Explanation:
    """.stripMargin)

    /* Pass/Fail status */
    ColorPrinter.printPassFail(pass = false)

    Duration.between(start,end)

  end run


  def providedMethod():Unit =
    ???

  end providedMethod


end Pb_Template