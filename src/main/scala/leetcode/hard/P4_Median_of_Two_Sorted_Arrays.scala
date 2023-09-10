package leetcode.hard

import leetcode.{ColorPrinter, LeetcodeProblem}

import java.time.{Duration, Instant, temporal}, temporal.Temporal

import scala.concurrent.duration.FiniteDuration


object P4_Median_of_Two_Sorted_Arrays extends LeetcodeProblem {

  val prob:Array[String] = this.getClass.getSimpleName.split('_')
  val probNum:String = prob.head.drop(1) //drop leading P
  val probName:String = prob.tail.mkString(" ").dropRight(1) //drop trailing $

  def findMedianSortedArrays(nums1:Array[Int], nums2:Array[Int]):Double = {
    0.0
  }

  def run():FiniteDuration = {
    /* Provided input */
    val arg1 = Array(1,2)
    val arg2 = Array(3,4)

    /* Expected output */
    val exp = 2.5

    /* Computed output with run time */
    val start = Instant.now()
    val sol = findMedianSortedArrays(arg1,arg2)
    val end = Instant.now()

    /* Problem description with computed output */
    print(f"""
      |Problem ${probNum}: ${probName}
      |Given two sorted arrays nums1 and nums2 of size m and n respectively, return the median of the two sorted arrays.
      |The overall run time complexity should be O(log (m+n)).
      |
      |Constraints:
      |* nums1.length == m
      |* nums2.length == n
      |* 0 <= m <= 1000
      |* 0 <= n <= 1000
      |* 1 <= m + n <= 2000
      |* -10^6 <= nums1[i], nums2[i] <= 10^6
      |
      |Input: nums1 = ${arg1.mkString("[",",","]")}, nums2 = ${arg2.mkString("[",",","]")}
      |Output: ${sol}
      |Expected: ${exp}
      |Explanation: merged array = [1,2,3,4] and median is (2+3)/2 = 2.5
    """.stripMargin)

    /* Pass/Fail status */
    ColorPrinter.printPassFail(pass = false)

    Duration.between(start,end)
  }
}
