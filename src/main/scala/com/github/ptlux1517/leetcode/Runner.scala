package com.github.ptlux1517.leetcode

import os.{walk, isFile}

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.io.StdIn.readLine


//LeetCode uses Scala 2.13.7
object Runner {

  private val problemClassPackageTuples:Seq[(String,String)] =
    os.walk(path=os.pwd/"src"/"main"/"scala"/"com"/"github"/"ptlux1517"/"leetcode")
    .filter(os.isFile(_))
    .map(_.toString)
    .map(_.stripPrefix(f"${os.pwd.toString}/src/main/scala/com/github/ptlux1517/leetcode/"))
    .map(_.stripSuffix(".scala"))
    .filter(s => List("easy","medium","hard").exists(s.startsWith))
    .map(_.split('/')) //split package and class
    .map(arr => (arr.last, arr.head)) //return tuple (class,package)
    .sorted //in order of problem num

  @main def run():Unit = {
    print("\nEnter the problem number: ")
    val problemNum:Int = readLine().toInt

    val (problemClass,problemDifficulty):(String,String) =
      problemClassPackageTuples
      .find(_._1.startsWith(f"P${problemNum}_"))
      .getOrElse({
        println(f"Problem $problemNum has not been implemented. Program exiting.")
        System.exit(1)
        ("","")
      })

    val clazz:Class[?] = Class.forName(f"com.github.ptlux1517.leetcode.$problemDifficulty.$problemClass$$")
    val problem:LeetcodeProblem = clazz.getField("MODULE$").get(clazz).asInstanceOf[LeetcodeProblem]

    val runTime:FiniteDuration = problem.run()

    println(f"\nExecution took ${runTime.toUnit(MILLISECONDS)} ms")
  }
}
