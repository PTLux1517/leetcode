package com.github.ptlux1517.leetcode

import java.time.{Duration, Instant, temporal}, temporal.Temporal

import scala.concurrent.duration.FiniteDuration


object Pb_Template extends LeetcodeProblem {

  val prob:Array[String] = this.getClass.getSimpleName.split('_')
  val probNum:String = prob.head.drop(1) //drop leading P
  val probName:String = prob.tail.mkString(" ").dropRight(1) //drop trailing $

  def providedMethod():Unit = {}

  def run():FiniteDuration = {
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
      |Problem ${probNum}: ${probName}
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
  }
}
