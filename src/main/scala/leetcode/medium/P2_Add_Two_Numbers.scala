package leetcode.medium

import leetcode.{ColorPrinter, LeetcodeProblem}

import java.time.{Duration, Instant, temporal}, temporal.Temporal

import scala.concurrent.duration.FiniteDuration


object P2_Add_Two_Numbers extends LeetcodeProblem {

  val prob:Array[String] = this.getClass.getSimpleName.split('_')
  val probNum:String = prob.head.drop(1) //drop leading P
  val probName:String = prob.tail.mkString(" ").dropRight(1) //drop trailing $

  class ListNode (_x:Int = 0, _next:ListNode = null) {
    var x:Int = _x
    var next:ListNode = _next

    private def traverse: StringBuilder => ListNode => String
                        = sb            => {
      case null => sb.append("]").toString()
      case ListNode(x,next) => traverse(sb append f"$x ")(next)
    }

    override def toString: String
                         = {
      traverse(StringBuilder() append "[ ")(this)
    }
  }

  object ListNode {
    def unapply(node:ListNode): Option[(Int,ListNode)] = Some(node.x,node.next)
  }

  def addTwoNumbers: ListNode => ListNode => ListNode
                   = l1       => l2       => solListBuilder(ListNode())(0)(Option(l1))(Option(l2))

  /*
      1    0    0 :carryPrev
      3 <- 4 <- 2 :l1
    + 4 <- 6 <- 5 :l2
    -------------
      8 <- 0 <- 7 :sol
  */
//  def solListBuilder: Int       => ListNode => ListNode => ListNode
//                    = carryPrev => l1       => l2       =>
//    (carryPrev,l1,l2) match {
//      /* Base case: one list (or neither) has remaining digits and there is no carry remaining to alter the rest of the list */
//      case (0,_,null) => l1
//      case (0,null,_) => l2
//      /* There is at least one more digit to add from either list or from the carry */
//      case _          => {
//        val l1NonNull = if l1==null then ListNode() else l1
//        val l2NonNull = if l2==null then ListNode() else l2
//        val sum = carryPrev + l1NonNull.x + l2NonNull.x
//        val (carryNext,rem) = (sum/10,sum%10)
//        val solNode = ListNode(rem)
//        solNode.next = solListBuilder(carryNext)(l1NonNull.next)(l2NonNull.next)
//        solNode
//      }
//    }

  def solListBuilder: ListNode             => Int       => Option[ListNode] => Option[ListNode] => ListNode|Null
                    = endOfListPlaceholder => carryPrev => l1               => l2               =>
    (carryPrev, l1, l2) match {
      /* Base case: one list (or neither) has remaining digits and there is no carry remaining to alter the rest of the list */
      case (0, _, None) => l1.orNull
      case (0, None, _) => l2.orNull
      /* There is at least one more digit to add from either list or from the carry */
      case _ => {
        val l1OrPH = l1.getOrElse(endOfListPlaceholder)
        val l2OrPH = l2.getOrElse(endOfListPlaceholder)
        val sum = carryPrev + l1OrPH.x + l2OrPH.x
        val (carryNext,rem) = (sum/10,sum%10)
        val solNode = ListNode(rem)
        solNode.next = solListBuilder(endOfListPlaceholder)(carryNext)(Option(l1OrPH.next))(Option(l2OrPH.next))
        solNode
      }
    }

//  def iterNodes(head:ListNode): Iterator[ListNode] =
//    Iterator.unfold[ListNode,ListNode](head)(node => if node==null then None else Some(node,node.next))
//
//  def iterVals(head:ListNode): Iterator[Int] =
//    iterNodes(head).map(_.x)
//
//  def addTwoNumbers(l1:ListNode, l2:ListNode): ListNode = {
//    val sumListHead = ListNode(0,null)
//    val (sumListTailPred,sumListTail) = iterVals(l1).zipAll(iterVals(l2),thisElem=0,thatElem=0).foldLeft((sumListHead,sumListHead))(op={
//      case ((sumNodePred,sumNode),(l1x,l2x)) =>
//        val sum = sumNode.x + l1x + l2x
//        val (carry,rem) = (sum/10,sum%10)
//        sumNode.x = rem
//        sumNode.next = ListNode(carry,null)
//        (sumNode,sumNode.next)
//    })
//    iterVals(sumListHead).foreach(v => print(f"$v "))
//    println()
//    if (sumListTail.x==0) then sumListTailPred.next = null
//    sumListHead
//  }

  def run():FiniteDuration = {
    /* Provided input */
    val arg1 = ListNode(2)
    arg1.next = ListNode(4)
    arg1.next.next = ListNode(3)

    val arg2 = ListNode(5)
    arg2.next = ListNode(6)
    arg2.next.next = ListNode(4)

    /* Expected output */
    val exp = "[ 7 0 8 ]"

    /* Computed output with run time */
    val start = Instant.now()
    val sol = addTwoNumbers(arg1)(arg2)
//    val sol = addTwoNumbers(arg1,arg2)
    val end = Instant.now()

    /* Problem description with computed output */
    print(f"""
      |Problem ${probNum}: ${probName}
      |You are given two non-empty linked lists representing two non-negative integers.
      |The digits are stored in reverse order, and each of their nodes contains a single digit.
      |Add the two numbers and return the sum as a linked list.
      |You may assume the two numbers do not contain any leading zero, except the number 0 itself.
      |
      |Constraints:
      |* The number of nodes in each linked list is in the range [1,100]
      |* 0<=Node.val<=9
      |* It is guaranteed that the list represents a number that does not have leading zeros
      |
      |Input: l1 = ${arg1}, l2 = ${arg2}
      |Output: ${sol}
      |Expected: ${exp}
      |Explanation: 342 + 465 = 807
    """.stripMargin)

    /* Pass/Fail status */
    ColorPrinter.printPassFail(pass = sol.toString==exp)

    Duration.between(start,end)
  }
}
