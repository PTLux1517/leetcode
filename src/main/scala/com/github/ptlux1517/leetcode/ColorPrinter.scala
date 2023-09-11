package com.github.ptlux1517.leetcode


object ColorPrinter {

   val ANSI_GREEN = "\u001b[32m"
   val ANSI_RED = "\u001b[31m"
   val ANSI_RESET = "\u001b[0m"

   def printPassFail(pass:Boolean): Unit =
     if (pass) println(f"\n${ANSI_GREEN}PASS${ANSI_RESET}")
     else println(f"\n${ANSI_RED}FAIL${ANSI_RESET}")
}
