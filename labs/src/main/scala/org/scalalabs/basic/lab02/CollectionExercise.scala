package org.scalalabs.basic.lab02
/**
 * This Lab contains exercises where the usage of
 * higher order collection methods can be rehearsed.
 */
import sys._
import scala.math
import scala.collection.mutable

object CollectionExercise01 {

  /**
   * Taken from: <a href="http://code.google.com/codejam/contest/1460488/dashboard">Problem A. Speaking in Tongues</a>
   *
   * Problem
   * The aim of this task is to translate a language into a new language called Googlerese.
   * To translate we take any message and replace each English letter with another English letter.
   * This mapping is one-to-one and onto, which means that the same input letter always gets replaced
   * with the same output letter, and different input letters always get replaced with different output letters.
   * A letter may be replaced by itself. Spaces are left as-is.
   *
   * For example (and here is a hint!), the translation algorithm includes the following three mappings:
   * 'a' -> 'y', 'o' -> 'e', and 'z' -> 'q'. This means that "a zoo" will become "y qee".
   *
   * Sample Input/Output
   * Input:
   * Case 1: ejp mysljylc kd kxveddknmc re jsicpdrysi
   * Case 2: rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd
   * Case 3: de kr kd eoya kw aej tysr re ujdr lkgc jv
   *
   * Output:
   * Case 1: our language is impossible to understand
   * Case 2: there are twenty six factorial possibilities
   * Case 3: so it is okay if you want to just give up
   *
   */
  private def translateChar(c: Char): Char = {
    c match {
      case 'a' => 'y'
      case 'b' => 'h'
      case 'c' => 'e'
      case 'd' => 's'
      case 'e' => 'o'
      case 'f' => 'c'
      case 'g' => 'v'
      case 'h' => 'x'
      case 'i' => 'd'
      case 'j' => 'u'
      case 'k' => 'i'
      case 'l' => 'g'
      case 'm' => 'l'
      case 'n' => 'b'
      case 'o' => 'k'
      case 'p' => 'r'
      case 'q' => '_'
      case 'r' => 't'
      case 's' => 'n'
      case 't' => 'w'
      case 'u' => 'j'
      case 'v' => 'p'
      case 'w' => 'f'
      case 'x' => 'm'
      case 'y' => 'a'
      case 'z' => '_'
      case ' ' => ' '
      case _ => '_'
    }
  }

  private def translateLine(line: String): String = {
    return line.map(translateChar(_))
  }

  def googleCodeJamGooglerese(lines: String*): Seq[String] = {
    return lines.map(translateLine(_)).toList
  }
}
/*========================================================== */

object CollectionExercise02 {

  class Person(val age: Int, val name: String)

  /**
   * Take a look at the java class: {@link ImperativeSample}. The
   * groupAdultsPerAgeGroup is implemented using an imperative programming
   * style.
   * Rewrite the method groupAdultsPerAgeGroup in the ImperativeSample java class
   * using a functional approach.
   */
  private def groupAdultByAget(groupId: Int, person: Person,
    groups: mutable.Map[Int, Seq[Person]]) {
    if (groups.contains(groupId)) {
      groups(groupId) :+= person
    } else {
      groups += (groupId -> Seq(person))
    }
  }

  def groupAdultsPerAgeGroup(persons: Seq[Person]): Map[Int, Seq[Person]] = {
    val groups = mutable.Map[Int, Seq[Person]]()
    // persons.foreach(groupPersonByAge(groups, _))
    persons.foreach(person => {
      person.age / 10 match {
        case 2 => groupAdultByAget(20, person, groups)
        case 3 => groupAdultByAget(30, person, groups)
        case 4 => groupAdultByAget(40, person, groups)
        case _ => ""
      }
    })
    return groups.toMap
  }
}

/*========================================================== */

object CollectionExercise03 {
  /**
   * Create a method that checks that each subsequent value is greater than
   * the previous one.
   * E.g.:
   * checkValuesIncrease(Seq(1,2,3)) == true
   * checkValuesIncrease(Seq(1,2,2)) == false
   */
  def checkValuesIncrease[T <% Ordered[T]](seq: Seq[T]): Boolean = {
    if ((seq.isEmpty) || (seq.length == 1))
      true
    else
      (seq.head < seq(1)) && checkValuesIncrease(seq.tail)
  }

}
/*========================================================== */

object CollectionExercise04 {
  /**
   * Calculate the length of the longest word in a list of sentences.
   * To keep it simple it's ok to use String.split to extract all words of a sentence.
   */
  def calcLengthLongestWord(lines: String*): Int = {
    var maxLen = 0;
    for (line <- lines) {
      val words = line.split(' ')
      val lineMax = words.foldLeft(0)((m, n) => math.max(m, n.length))
      if (lineMax > maxLen)
        maxLen = lineMax
    }
    return maxLen;
  }
}

/*========================================================== */

object CollectionExercise05 {
  /**
   * Filter all even numbers of the given sequence using foldLeft.
   * E.g. Seq(1,2,3) is Seq(2)
   */
  def filterWithFoldLeft(seq: Seq[Int]): Seq[Int] = {
    seq.foldLeft(Seq[Int]()) {
      (m: Seq[Int], n: Int) => if (n % 2 == 0) m :+ n else m
    }
  }

  /**
   * Group all numbers based on whether they are even or odd using foldLeft.
   * For even use 'true' for odd use 'false'.
   * E.g: Seq(1,2,3) is Map(0 -> Seq(2), 1 -> Seq(1,3))
   */
  def groupByWithFoldLeft(seq: Seq[Int]): Map[Boolean, Seq[Int]] = {
    val even = seq.foldLeft(Seq[Int]()) {
      (m: Seq[Int], n: Int) => if (n % 2 == 0) m :+ n else m
    }
    val odd = seq.foldLeft(Seq[Int]()) {
      (m: Seq[Int], n: Int) => if (n % 2 == 1) m :+ n else m
    }
    Map((false -> odd), (true -> even))
  }
}

