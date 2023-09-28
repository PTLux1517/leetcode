package com.github.ptlux1517.leetcode

import scala.Console.{GREEN,RED,RESET}


object ColorPrinter {
   def printPassFail(pass:Boolean):Unit =
     if (pass) println(f"\n${GREEN}PASS${RESET}")
     else println(f"\n${RED}FAIL${RESET}")
}
