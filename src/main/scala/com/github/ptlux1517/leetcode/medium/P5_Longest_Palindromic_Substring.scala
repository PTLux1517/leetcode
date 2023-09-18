package com.github.ptlux1517.leetcode.medium

import com.github.ptlux1517.leetcode.{ColorPrinter, LeetcodeProblem}

import java.time.{Duration, Instant, temporal}
import temporal.Temporal
import scala.concurrent.duration.FiniteDuration


object P5_Longest_Palindromic_Substring extends LeetcodeProblem {

  val prob:Array[String] = this.getClass.getSimpleName.split('_')
  val probNum:String = prob.head.drop(1) //drop leading P
  val probName:String = prob.tail.mkString(" ").dropRight(1) //drop trailing $

  private enum PalindromeType {case
    ConsecutiveChar,
    SingleInflectionPoint,
    DoubleInflectionPoint
  }

  private case class LongestPalindromeInfo(
    var palType:PalindromeType = PalindromeType.ConsecutiveChar, //determines how to use the idx for the pal string retrieval at the end
    var len:Int = 0, //default is 1 since s is constrained to min length 1 and a single char forms a trivial palindrome
    var idx:Int = 0 //default is located at idx 0 since there is always at least a trivial palindrome there
  )

  def longestPalindrome(s:String):String = {

    val sol = LongestPalindromeInfo()
    var longestPal = s.take(1) //default return

    val arr = s.toArray
    val sLen = s.length

    var consecPtr = 0
    var longestShoulderLen = 0
    var twoElemInfPtOffset = 0

    var currLen = 0

    for
      i <- s.indices.dropRight(1)
      if 0 <= i-longestShoulderLen && i+twoElemInfPtOffset+longestShoulderLen < sLen //only check i if it can produce a longer palindrome
    do {
      var consecCt = 0
      if consecPtr <= i then {
        while
          consecCt += 1 //count current char
          consecPtr += 1
          consecPtr<sLen && arr(consecPtr)==arr(consecPtr-1)
        do {}
      }
      if consecCt > sol.len then {
        sol.palType = PalindromeType.ConsecutiveChar
        sol.len = consecCt
        sol.idx = consecPtr - consecCt
        longestShoulderLen = if consecCt%2 == 1 then (consecCt-1)/2 else (consecCt-2)/2
        twoElemInfPtOffset = 0
      }
      else {
        var prevIdx = i-longestShoulderLen-1
        var nextIdx = i+longestShoulderLen+1
        if 0<=prevIdx && nextIdx<sLen && arr(prevIdx)==arr(nextIdx) then {
          currLen = lengthOfPalindromeCenteredAtInflectionPoint(i,twoElemInflectionPoint=false,arr,sLen)
          if currLen > sol.len then {
            sol.palType = PalindromeType.SingleInflectionPoint
            sol.len = currLen
            sol.idx = i
            longestShoulderLen = if currLen%2 == 1 then (currLen-1)/2 else (currLen-2)/2
            twoElemInfPtOffset = 0
          }
        }
        prevIdx = i-longestShoulderLen
        nextIdx = i+1+longestShoulderLen
        if 0<=prevIdx && nextIdx<sLen && arr(prevIdx)==arr(nextIdx) then {
          currLen = lengthOfPalindromeCenteredAtInflectionPoint(i,twoElemInflectionPoint=true,arr,sLen)
          if currLen > sol.len then {
            sol.palType = PalindromeType.DoubleInflectionPoint
            sol.len = currLen
            sol.idx = i
            longestShoulderLen = if currLen%2 == 1 then (currLen-1)/2 else (currLen-2)/2
            twoElemInfPtOffset = 1
          }
        }
      }
      println(f"($i,$consecPtr): ${sol}")
    }
    if sol.len == 0 then longestPal else retrieveLongestPalindromeSubstring(s, sol)
  }

  private def lengthOfPalindromeCenteredAtInflectionPoint(inflectionIdx:Int, twoElemInflectionPoint:Boolean, s:Array[Char], len:Int):Int = {
    var palLengthCounter = if twoElemInflectionPoint then 2 else 1

    val revIndices = (inflectionIdx to 0 by -1).drop(1)
    val fwdIndices =
      (if twoElemInflectionPoint
        then inflectionIdx+1 until len
        else inflectionIdx until len).drop(1)

    val indices = revIndices zip fwdIndices //to truncate the longer of the two

    for (r,f) <- indices do
      if s(r)==s(f) then palLengthCounter += 2

    palLengthCounter
  }

  private def retrieveLongestPalindromeSubstring(s:String, info:LongestPalindromeInfo):String = {
    info.palType match
      case PalindromeType.ConsecutiveChar =>
        s.substring(info.idx, info.idx+info.len)
      case PalindromeType.SingleInflectionPoint =>
        val shoulderLen = (info.len-1) / 2
        s.substring(info.idx-shoulderLen, info.idx+shoulderLen+1)
      case PalindromeType.DoubleInflectionPoint =>
        val shoulderLen = (info.len-2) / 2
        s.substring(info.idx-shoulderLen, info.idx+1+shoulderLen+1)
  }

//  private def arePalindromeBounds(s:Array[Char], lIdx:Int, rIdx:Int):Boolean = {
//    val fwdIter = s.iterator
//    val revIter = s.reverseIterator
//    ???
//  }

  def run():FiniteDuration = {
    /* Provided input */
//    val arg1 = "babad"
    val arg1 = "abcdee"

    /* Expected output */
//    val exp = "bab"
    val exp = "ee"

    /* Computed output with run time */
    val start = Instant.now()
    val sol = longestPalindrome(arg1)
    val end = Instant.now()

    /* Problem description with computed output */
    print(f"""
      |Problem ${probNum}: ${probName}
      |Given a string s, return the longest palindromic substring in s.
      |
      |Constraints:
      |* 1 <= s.length <= 1000
      |* s consist of only digits and English letters.
      |
      |Input: ${arg1}
      |Output: ${sol}
      |Expected: ${exp}
      |Explanation: "aba" is also a valid answer
    """.stripMargin)

    /* Pass/Fail status */
    ColorPrinter.printPassFail(pass = sol==exp)

    Duration.between(start,end)
  }
}
