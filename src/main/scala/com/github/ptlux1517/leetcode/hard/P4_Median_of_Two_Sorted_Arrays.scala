package com.github.ptlux1517.leetcode.hard

import com.github.ptlux1517.leetcode.{ColorPrinter, LeetcodeProblem}

import java.time.{Duration, Instant, temporal}, temporal.Temporal

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration


object P4_Median_of_Two_Sorted_Arrays extends LeetcodeProblem:

  val className:Array[String] = this.getClass.getSimpleName.split('_')
  val probNum:String = className.head.drop(1) //drop leading P
  val probName:String = className.tail.mkString(" ").dropRight(1) //drop trailing $


  def run():FiniteDuration =
    /* Provided input */
    val arg1 = Array(1,2)
    val arg2 = Array(3,4)
//    val arg1 = Array(1,2,3,7,8,9,13,14,15)
//    val arg2 = Array(4,5,6,10,11,12,16,17,18)

    /* Expected output */
    val exp = 2.5
//    val exp = 9.5

    /* Computed output with run time */
    val start = Instant.now()
    val sol = findMedianSortedArrays(arg1,arg2)
    val end = Instant.now()

    /* Problem description with computed output */
    print(f"""
      |Problem $probNum: $probName
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
    ColorPrinter.printPassFail(pass = sol==exp)

    Duration.between(start,end)

  end run


  def findMedianSortedArrays(nums1:Array[Int], nums2:Array[Int]):Double =
    val combinedLength = nums1.length + nums2.length
    if combinedLength < 100 then return medianOne((nums1 ++ nums2).sorted) //to circumvent LeetCode timer issues for small m + n
    val singleMedian = combinedLength%2 == 1
    val stepsToMedian = if singleMedian then combinedLength/2 + 1 else combinedLength/2
    classifyRelation(nums1,nums2) match
    case (_, Relation.Empty)                               => medianOne(nums1)
    case (Relation.Empty, _)                               => medianOne(nums2)
    case (Relation.DisjointBefore, Relation.DisjointAfter) => medianTwoDisjoint(numsBefore=nums1, numsAfter=nums2, singleMedian, stepsToMedian)
    case (Relation.DisjointAfter, Relation.DisjointBefore) => medianTwoDisjoint(numsBefore=nums2, numsAfter=nums1, singleMedian, stepsToMedian)
    case _                                                 => binaryApproachMedian(
      nums1, nums2, i1 = -1, i2 = -1, singleMedian, stepsToMedian, stepsTaken=0
    )
  end findMedianSortedArrays


  @tailrec
  private def binaryApproachMedian(
    nums1:Array[Int], nums2:Array[Int], i1:Int, i2:Int,
    singleMedian:Boolean, stepsToMedian:Int, stepsTaken:Int
  ):Double =
    val remainingSteps = stepsToMedian - stepsTaken
    if remainingSteps == 0
    then
      val greaterElem = (i1,i2) match
        case (_,-1) => nums1(i1)
        case (-1,_) => nums2(i2)
        case _      => nums1(i1) max nums2(i2)
      if singleMedian
      then greaterElem
      else
        val nextGreatestElem = (nums1.length,nums2.length) match
          case (l1,l2) if i1+1 < l1 && i2+1 < l2 => nums1(i1+1) min nums2(i2+1)
          case (l1,l2) if i1+1 < l1 => nums1(i1+1)
          case (l1,l2) if i2+1 < l2 => nums2(i2+1)
        (greaterElem + nextGreatestElem) / 2d
    else
      val binaryJump = if remainingSteps==1 then 1 else remainingSteps/2
      val jumpIdx1 = i1+binaryJump min nums1.length-1
      val jumpIdx2 = i2+binaryJump min nums2.length-1
      if nums1(jumpIdx1) <= nums2(jumpIdx2)
      then binaryApproachMedian(nums1, nums2, jumpIdx1, i2, singleMedian, stepsToMedian, stepsTaken+(jumpIdx1-i1))
      else binaryApproachMedian(nums1, nums2, i1, jumpIdx2, singleMedian, stepsToMedian, stepsTaken+(jumpIdx2-i2))
  end binaryApproachMedian


  private def mid(low:Int, high:Int):Int = low + (high-low)/2


  private def medianOne(nums:Array[Int]):Double =
    nums.length match
    case len if len%2 == 1 =>
      val midC = mid(0,len-1)
      nums(midC)
    case len if len%2 == 0 =>
      val midL = mid(0,len-1)
      val midR = midL+1
      (nums(midL)+nums(midR)) / 2d
  end medianOne


  private def medianTwoDisjoint(
    numsBefore:Array[Int], numsAfter:Array[Int], singleMedian:Boolean, stepsToMedian:Int
  ):Double =
    if stepsToMedian <= numsBefore.length
    then //start of median is within nums1
      if singleMedian
      then numsBefore(stepsToMedian-1)
      else //two element median could span both arrays
        if stepsToMedian == numsBefore.length
        then (numsBefore.last + numsAfter.head) / 2d //two element median spans both arrays
        else (numsBefore(stepsToMedian-1) + numsBefore(stepsToMedian)) / 2d //two element median is within nums1
    else //start of median is within nums2
      if singleMedian
      then numsAfter(stepsToMedian-numsBefore.length-1)
      else
        val idx = stepsToMedian-numsBefore.length-1
        (numsAfter(idx) + numsAfter(idx+1)) / 2d
  end medianTwoDisjoint


  private enum Relation:
    case
    Empty,
    EqualBounds,
    Contains, Within,
    DisjointBefore, DisjointAfter,
    OverlapBefore, OverlapAfter


  private def classifyRelation(nums1:Array[Int], nums2:Array[Int]):(Relation,Relation) =
    (nums1.length, nums2.length) match
    case (_,0) => (Relation.DisjointBefore, Relation.Empty)
    case (0,_) => (Relation.Empty, Relation.DisjointAfter)
    case _     =>
      (nums1.head, nums1.last, nums2.head, nums2.last) match
      case (lo1,hi1,lo2,hi2) if hi1<=lo2 => (Relation.DisjointBefore, Relation.DisjointAfter)
      case (lo1,hi1,lo2,hi2) if hi2<=lo1 => (Relation.DisjointAfter, Relation.DisjointBefore)
      case (lo1,hi1,lo2,hi2) if lo1==lo2 && hi1==hi2 => (Relation.EqualBounds, Relation.EqualBounds)
      case (lo1,hi1,lo2,hi2) if lo1<=lo2 && hi1>=hi2 => (Relation.Contains, Relation.Within)
      case (lo1,hi1,lo2,hi2) if lo1>=lo2 && hi1<=hi2 => (Relation.Within, Relation.Contains)
      case (lo1,hi1,lo2,hi2) if lo1<=lo2 && hi1<=hi2 => (Relation.OverlapBefore, Relation.OverlapAfter)
      case (lo1,hi1,lo2,hi2) if lo1>=lo2 && hi1>=hi2 => (Relation.OverlapAfter, Relation.OverlapBefore)
  end classifyRelation


end P4_Median_of_Two_Sorted_Arrays