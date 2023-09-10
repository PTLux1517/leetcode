package leetcode

import leetcode._, easy._, medium._, hard._

import java.nio.file.{Files, Paths, Path}

import scala.concurrent.duration.*
import scala.io.StdIn.readLine
import scala.jdk.CollectionConverters.*
import scala.jdk.DurationConverters.*


//LeetCode uses Scala 2.13.7
object Runner {

  private val projectDir:Path = Paths.get("").toAbsolutePath
  private val packageDir:Path = projectDir.resolve("src/main/scala/leetcode")
  private val problemDirs = List("easy", "medium", "hard")

  private val problemTuples:Seq[(String,String)] =
    Files.walk(packageDir).iterator().asScala
      .filter(Files.isRegularFile(_))
      .map(_.toString.split("leetcode/").last) //get rid of path prefix /home/corytomlinson/ScalaProjects/LeetCode/src/main/scala/leetcode/
      .filter(s => problemDirs.exists(s.startsWith)) //only list files in problem dirs
      .map(_.split('.').head) //get rid of suffix .scala
      .map(_.split('/')) //split problemDir from problem class
      .map(arr => (arr.last, arr.head)) //return tuple (problemClass,difficulty)
      .toSeq.sorted //in order of problem num



  @main def run():Unit = {
    print("\nEnter the problem number: ")
    val problemNum:Int = readLine().toInt

    val (problemClass,problemDifficulty):(String,String) =
      problemTuples
      .find(_._1.contains(problemNum.toString))
      .getOrElse({
        println(f"Problem $problemNum has not been implemented. Program exiting.")
        System.exit(1)
        ("","")
      })

    val clazz:Class[?] = Class.forName(f"leetcode.$problemDifficulty.$problemClass$$")
    val problem:LeetcodeProblem = clazz.getField("MODULE$").get(clazz).asInstanceOf[LeetcodeProblem]

    val runTime:FiniteDuration = problem.run().toScala

    println(f"\nExecution took ${runTime.toUnit(MILLISECONDS)} ms")
  }
}
