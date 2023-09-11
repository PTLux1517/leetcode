package com.github.ptlux1517.leetcode

import java.time.Duration

import scala.concurrent.duration.FiniteDuration
import scala.jdk.DurationConverters.JavaDurationOps


trait LeetcodeProblem {
  given Conversion[Duration,FiniteDuration] = _.toScala
  def run():FiniteDuration
}
