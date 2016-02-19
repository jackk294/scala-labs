package org.scalalabs.basic.lab03
import sys._
/**
 * This exercise introduces you to pattern matching in combination with recursion.
 *
 * Recursion is a key concept for the functional style programming.
 * In the exercises below you learn how to apply recursion in combination with Scala's pattern matching facilities.
 *
 * For this exercise exclusively use pattern matching constructs in order to make the corresponding unittest work.
 *
 * Reference material to solve these exercises can be found here:
 * Pattern matching in general: http://programming-scala.labs.oreilly.com/ch03.html#PatternMatching
 * Pattern matching and recursion: http://programming-scala.labs.oreilly.com/ch08.html#Recursion
 */

object RecursionPatternMatchingExercise {

  /**
   * ***********************************************************************
   * Recursive algorithms with pattern matching
   * For expected solution see unittest @RecursionPatternMatchingExerciseTest
   * ***********************************************************************
   */
  /**
   * Create a method that checks that each subsequent value is greater than
   * the previous one.
   * E.g.:
   * checkValuesIncrease(Seq(1,2,3)) == true
   * checkValuesIncrease(Seq(1,2,2)) == false
   */
  def checkValuesIncrease(seq: Seq[Int]): Boolean = {
    seq match {
      case Nil => true
      case s if s.length == 1 => true
      case s => s(0) < s(1) && checkValuesIncrease(s.tail)
    }
  }

  /**
   * Group Consecutive values
   * List(1,1,2,3,1,1) -> List(1,1), List(2), List(3), List(1,1)
   */
  def groupConsecutive[T](in: List[T]): List[List[T]] = {
    in match {
      case Nil => Nil
      case l if (l.length) == 1 => List(l)
      case l => {
        val head = l.head // T
        val tgroup = groupConsecutive(l.tail) // List[List[T]]
        val thead = tgroup.head // List[T]
        if (head == thead.head) {
          (head +: thead) :: tgroup.tail
        } else {
          List(head) :: tgroup
        }
      }
    }
  }

  /**
   * Group Equal values
   * List(1,1,2,3,1,1) -> List(1,1,1,1), List(2), List(3)
   */
  def groupEquals[T](in: List[T]): List[List[T]] = {
    in match {
      case Nil => Nil
      case l if (l.length) == l => List(l)
      case l => {
        val head = l.head
        val group = groupEquals(l.tail) // List[List[T]]

        var spIdx = 0
        for (item <- group) yield {
          if (head == item.head) {
            val (spHead, spTail) = group.splitAt(spIdx)
            return spHead ::: ((head +: item) :: spTail.tail)
          } else {
            spIdx += 1
          }
        }

        return group :+ List(head)
      }
    }
  }

  /**
   * Compress values
   * List(1,1,2,3,1,1) -> List(1,2,3)
   */
  def compress[T](in: List[T]): List[T] = {
    in match {
      case Nil => Nil
      case l => {
        val head = l.head
        val zipV = compress(l.tail) // List[T]

        for (item <- zipV) yield {
          if (head == item) {
            return zipV
          }
        }

        return head +: zipV
      }
    }
  }

  /**
   * Define the amount of all equal members
   * List(1,1,2,3,1,1) -> List((4,1),(1,2),(1,3))
   */
  def amountEqualMembers[T](in: List[T]): List[(Int, T)] = {
    in match {
      case Nil => Nil
      case l => {
        val head = l.head
        val zipV = amountEqualMembers(l.tail) // List[(Int, T)]

        var spIdx = 0
        for ((amount, value) <- zipV) {
          if (head == value) {
            val (spHead, spTail) = zipV.splitAt(spIdx)
            return (spTail.head._1 + 1, spTail.head._2) :: spHead ::: spTail.tail
          } else {
            spIdx += 1
          }
        }

        return (1, head) +: zipV
      }
    }
  }

  /**
   * Zip multiple lists
   * List(List(1,2,3), List('A, 'B, 'C), List('a, 'b, 'c)) -> List(List(1, 'A, 'a), List(2, 'B, 'b), List(3, 'C, 'c))
   */
  def zipMultiple(in: List[List[_]]): List[List[_]] = {
    in match {
      case Nil => Nil
      case l => {
        val rHead = for {
          item <- l
          if !item.isEmpty
        } yield item.head
        val rTail = for {
          item <- l
          if !item.isEmpty
        } yield item.tail

        if (!rHead.isEmpty) {
          rHead +: zipMultiple(rTail)
        } else Nil
      }
    }
  }

  /**
   * Zip multiple lists with different sizes
   * List(List(1), List('A, 'B, 'C), List('a, 'b)) -> List(List(1, 'A, 'a))
   */
  def zipMultipleWithDifferentSize(in: List[List[_]]): List[List[_]] = {
    in match {
      case Nil => Nil
      case l => {
        var rHead: List[Any] = List()
        var rTail: List[List[Any]] = List()

        for (item <- l) {
          if (item.isEmpty) return Nil

          rHead = rHead :+ item.head
          rTail = rTail :+ item.tail
        }

        rHead +: zipMultipleWithDifferentSize(rTail)
      }
    }
  }

}
