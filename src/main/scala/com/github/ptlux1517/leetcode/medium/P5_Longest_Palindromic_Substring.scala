package com.github.ptlux1517.leetcode.medium

import com.github.ptlux1517.leetcode.{ColorPrinter, LeetcodeProblem}

import java.time.{Duration, Instant, temporal}, temporal.Temporal

import scala.concurrent.duration.FiniteDuration


object P5_Longest_Palindromic_Substring extends LeetcodeProblem:

  val className:Array[String] = this.getClass.getSimpleName.split('_')
  val probNum:String = className.head.drop(1) //drop leading P
  val probName:String = className.tail.mkString(" ").dropRight(1) //drop trailing $


  def run():FiniteDuration =
    /* Provided input */
    val arg1 = "babad"

    /* Expected output */
    val exp = "bab"

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

  end run


  private enum PalindromeKind:
    case
    CONSECUTIVE_CHAR,
    SINGLE_MID_POINT,
    DOUBLE_MID_POINT
  import PalindromeKind.*


  private case class LongestPalindromeInfo(
    var kind:PalindromeKind = CONSECUTIVE_CHAR, //determines how to use the idx for the pal string retrieval at the end
    var idx:Int = 0, //default is located at idx 0 since there is always at least a trivial palindrome there
    var len:Int = 0,
    var shoulderLen:Int = 0,
    var midPtOffset:Int = 0
  ):
    def updateShoulderLenAndMidPtOffset():LongestPalindromeInfo =
      shoulderLen = if len%2 == 1
        then (len-1)/2
        else (len-2)/2
      midPtOffset = kind match
        case CONSECUTIVE_CHAR
           | SINGLE_MID_POINT => 0
        case DOUBLE_MID_POINT => 1
      this


  def longestPalindrome(s:String):String =

    val sArr = s.toArray
    val sLen = s.length

    var sol = LongestPalindromeInfo()
    var currLen = 0
    var consecPtr = 0

    for
      i <- s.indices.dropRight(1)
      if 0 <= i-sol.shoulderLen && i+sol.midPtOffset+sol.shoulderLen < sLen //only check i if it can produce a longer palindrome within bounds
    do
      /* Count any consecutive chars */
      var consecCt = 0
      if consecPtr <= i //allow i to catch back up in order to not count the same char multiple times in future iterations
      then while
        consecCt += 1 //count current char
        consecPtr += 1
        consecPtr<sLen && sArr(consecPtr)==sArr(consecPtr-1)
      do {}

      if consecCt > sol.len
      then sol = sol.copy(kind=CONSECUTIVE_CHAR, idx=consecPtr-consecCt, len=consecCt).updateShoulderLenAndMidPtOffset()
      else
        /* Check if a larger single mid point palindrome exists */
        var prevIdx = i-sol.shoulderLen-1
        var nextIdx = i+sol.shoulderLen+1
        if 0<=prevIdx && nextIdx<sLen && sArr(prevIdx)==sArr(nextIdx)
        then
          currLen = lengthOfPalindromeGivenMidPoint(i,twoElemMidPt=false,sArr,sLen)
          if currLen > sol.len
          then sol = sol.copy(kind=SINGLE_MID_POINT, idx=i, len=currLen).updateShoulderLenAndMidPtOffset()
        /* Check if a larger double mid point palindrome exists */
        prevIdx = i-sol.shoulderLen
        nextIdx = i+1+sol.shoulderLen
        if 0<=prevIdx && nextIdx<sLen && sArr(prevIdx)==sArr(nextIdx)
        then
          currLen = lengthOfPalindromeGivenMidPoint(i,twoElemMidPt=true,sArr,sLen)
          if currLen > sol.len
          then sol = sol.copy(kind=DOUBLE_MID_POINT, idx=i, len=currLen).updateShoulderLenAndMidPtOffset()
    end for

    if sol.len == 0 then s.take(1) else retrieveLongestPalindromeSubstring(s, sol)

  end longestPalindrome


  private def lengthOfPalindromeGivenMidPoint(midPtIdx:Int, twoElemMidPt:Boolean, s:Array[Char], len:Int):Int =

    if twoElemMidPt && s(midPtIdx)!=s(midPtIdx+1)
    then return 0

    var palLengthCounter = if twoElemMidPt then 2 else 1

    val revIndices = midPtIdx to 0 by -1 drop 1
    val fwdIndices = if twoElemMidPt
      then midPtIdx+1 until len drop 1
      else midPtIdx until len drop 1

    val indices = revIndices zip fwdIndices //to truncate the longer of the two

    for (r,f) <- indices
    do if s(r)==s(f) then palLengthCounter += 2

    palLengthCounter

  end lengthOfPalindromeGivenMidPoint


  private def retrieveLongestPalindromeSubstring(s:String, pal:LongestPalindromeInfo):String =
    pal.kind match
      case CONSECUTIVE_CHAR => s.substring(pal.idx                  , pal.idx + pal.len)
      case SINGLE_MID_POINT
         | DOUBLE_MID_POINT => s.substring(pal.idx - pal.shoulderLen, pal.idx + pal.midPtOffset + pal.shoulderLen + 1)

  end retrieveLongestPalindromeSubstring


end P5_Longest_Palindromic_Substring