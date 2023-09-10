package leetcode.medium

import leetcode.{ColorPrinter, LeetcodeProblem}

import java.time.{Duration, Instant, temporal}, temporal.Temporal

import scala.concurrent.duration.FiniteDuration


object P3_Longest_Substring_Without_Repeating_Characters extends LeetcodeProblem {

  val prob:Array[String] = this.getClass.getSimpleName.split('_')
  val probNum:String = prob.head.drop(1) //drop leading P
  val probName:String = prob.tail.mkString(" ").dropRight(1) //drop trailing $
  
//  def lengthOfLongestSubstring(s: String): Int = {
//    val list = s.scanLeft("")(op=
//      (currStr,currChar) => {
//        val res = currStr.substring(currStr.indexOf(currChar)+1) + currChar
//        print(f"($currStr + $currChar -> $res) ")
//        res
//      }
//    )
////    println(list)
//    list.map(_.length)
//    .max
//  }

  def lengthOfLongestSubstring(s:String):Int = {
    class Acc(var substr:String = "", var maxLen:Int = 0)
    val res:Acc = s.foldLeft(new Acc)((acc, currChar) => {
      val repeatIdx = acc.substr.indexOf(currChar)
      if (repeatIdx >= 0) {
        acc.substr = acc.substr.substring(repeatIdx+1)
      }
      acc.substr += currChar
      acc.maxLen = acc.maxLen max acc.substr.length
      acc
    })
    res.maxLen
  }

  def run():FiniteDuration = {
    /* Provided input */
//    val arg1 = "pwwkew"
    val arg1 = "dvdf"

    /* Expected output */
    val exp = 3

    /* Computed output with run time */
    val start = Instant.now()
    val sol = lengthOfLongestSubstring(arg1)
    val end = Instant.now()

    /* Problem description with computed output */
    print(f"""
      |Problem ${probNum}: ${probName}
      |Given a string s, find the length of the longest substring without repeating characters.
      |
      |Constraints:
      |* 0 <= s.length <= 5 * 10^4
      |* s consists of English letters, digits, symbols and spaces
      |
      |Input: ${arg1}
      |Output: ${sol}
      |Expected: ${exp}
      |Explanation: The answer is "wke", with the length of 3.
      |  Notice that the answer must be a substring, "pwke" is a subsequence and not a substring.
    """.stripMargin)

    /* Pass/Fail status */
    ColorPrinter.printPassFail(pass = sol==exp)

    Duration.between(start,end)
  }
}
