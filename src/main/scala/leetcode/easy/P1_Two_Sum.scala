package leetcode.easy

import leetcode.{ColorPrinter, LeetcodeProblem}

import scala.collection.mutable
import java.time.{Duration, Instant, temporal}, temporal.Temporal
import scala.util.boundary, boundary.break


object P1_Two_Sum extends LeetcodeProblem {

  val prob:Array[String] = this.getClass.getSimpleName.split('_')
  val probNum:String = prob.head.drop(1) //drop leading P
  val probName:String = prob.tail.mkString(" ").dropRight(1) //drop trailing $

  def twoSum: Array[Int] => Int    => Array[Int]
            = nums       => target => {
    val desiredComplementToPrevIdxMap = mutable.Map[Int,Int]()
    boundary:
      for ((elem,idx) <- nums.zipWithIndex) {
        desiredComplementToPrevIdxMap.get(elem) match
          case Some(prevIdx) => break(Array(prevIdx,idx)) //current elem is a compliment we were looking for previously
          case None => desiredComplementToPrevIdxMap.put(target-elem,idx)
      }
      Array.empty
  }

  def run():Duration = {
    /* Provided input */
    val arg1 = Array(2,7,11,15)
    val arg2 = 9

    /* Expected output */
    val exp = Array(0,1)

    /* Computed output with run time */
    val start = Instant.now()
    val sol = twoSum(arg1)(arg2)
    val end = Instant.now()

    /* Problem description with computed output */
    print(f"""
      |Problem ${probNum}: ${probName}
      |Given an array of integers nums and an integer target, return indices of the two numbers
      |such that they add up to target. You may assume that each input would have exactly one
      |solution, and you may not use the same element twice. You can return the answer in any order.
      |
      |Constraints:
      |* 2<=nums.length<=10^4
      |* -10^9<=nums[i]<=10^9
      |* -10^9<=target<=10^9
      |* Only one valid answer exists
      |
      |Input: nums = ${arg1.mkString("[",",","]")}, target = ${arg2}
      |Output: ${sol.mkString("[",",","]")}
      |Expected: ${exp.mkString("[",",","]")}
      |Explanation: Because nums(0) + nums(1) == 9, we return [0,1]
    """.stripMargin)

    /* Pass/Fail status */
    val solSet = sol.toSet
    val expSet = exp.toSet
    ColorPrinter.printPassFail(pass = solSet==expSet)

    Duration.between(start,end)
  }
}
