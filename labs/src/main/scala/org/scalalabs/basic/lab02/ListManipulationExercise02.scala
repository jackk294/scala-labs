package org.scalalabs.basic.lab02

import scala.collection.mutable.ListBuffer
import scala.math
import sys._

object ListManipulationExercise02 {

  /**
   * Find the maximum element in a list, e.g. maxElementInList(List(1,9,3,5)) == 9
   * As usual, various ways exist: pattern matching, folding, ...
   */
  def maxElementInList(l: List[Int]): Int = {
    l.foldLeft(0)((m, n) => math.max(m, n))
  }

  /**
   * Calculate the sum of the equally position elements
   * of the two list
   */
  def sumOfTwo(l1: List[Int], l2: List[Int]): List[Int] = {
    val len = math.max(l1.length, l2.length)
    val list = for (i <- 0 to len - 1) yield {
      var item = 0
      if (l1.isDefinedAt(i)) item += l1(i)
      if (l2.isDefinedAt(i)) item += l2(i)
      item
    }
    list.toList
  }

  /**
   *  For this exercise preferably make use of the sumOfTwo
   * method above
   */
  def sumOfMany(l: List[Int]*): List[Int] = {
    l.foldLeft(List[Int]())((m, n) => sumOfTwo(m, n))
  }

  case class Person(age: Int, firstName: String, lastName: String)

  /**
   * The following method is implemented in the most in-elegant way we could think of.
   * The idea is to re-write the method into more functional style. In the end, you
   * may be able to achieve the same functionality as implemented below
   * in a one-liner.
   */
  def separateTheMenFromTheBoys(persons: List[Person]): List[List[String]] = {
    val groups = persons.groupBy(person => if (person.age < 18) 1 else 0)
    groups.map(group => (group._1, group._2.sortWith((m, n) => m.age < n.age))).map(group => group._2.map(_.firstName)).toList
  }

}
