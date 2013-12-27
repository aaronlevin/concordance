package org.weirdcanada.concordance

// scalaz imports
import scalaz._
import Scalaz._
import iteratee._
import Iteratee._
import effect._

// Java imports
import java.io._


/**
 * We will solve the conordance problem in two ways:
 *
 * 1. Naive
 *    In this solution we take the most naive approach. Split a sentance on '.'
 *   and words on ' ' and fold-left into oblivion. It is naive for a number of
 *   reasons:
 *     - not all words are separated by spaces (compound words and things with
 *     hyphens).
 *     - not all sentances are separated by periods (e.g. semi-colons)
 *     - we aren't cleaning any of the data for punctuation.
 *     - this is all done in memory.
 *
 * 2. Constant Memory (scalaz Iteratee)
 *    In this solution, we assume that we're solving concordance on a huge text
 *    file or other buffered input. We use pure functional Iteratee's, which are
 *    a basic Free Monadic implementation of a function that steps through the
 *    input in constant memory. There are several advantages to Iteratees
 *    (mainly that they can be composed) and disadvantages (they are relatively
 *    complex and obtuse to use). 
 */

object Concordance {

  /**
   * Simple ADTs for the naive approach and constant memory approach
   */
  case class SentanceWordCount(word: String, total: Int, lineCounts: List[(Int,Int)]) {
    override def toString: String = 
      ItemCount[String](word, total, lineCounts.map { case (a,b) => (a.toLong,b) }.toMap).toString
  }
  case class ItemCount[A](word: A, total: Int, lineCounts: Map[Long, Int]) {
    override def toString: String = """%s:
  total count: %s
%s
""".format(word, total, lineCounts.map { case (a,b) => "  sentance #%s : %s".format(a,b) }.mkString("\n"))
  }


  /**
   * An Iteratee that reads from a BufferedReader
   */
  val fileReader: EnumeratorT[Char, IO] =
    enumReader[IO](
      new BufferedReader (new FileReader("./text.txt"))
    )
    .map {_.valueOr { ' ' } }

  /**
   * An Iteratee to read characters from a file until you hit a period. We do 
   * the weird flatMap so that we can call `sequenceI` on this and it won't
   * iterate forever.
   */
  val sentanceIteratee: IterateeT[Char, IO, String] = 
    takeWhile[Char, List]( _ != '.' )
      .flatMap { ln => drop[Char, Id](1).map { _ => ln } }
      .map { _.mkString }
      .up[IO]

  val alphaNumericRegex = "[^\\w]".r

  /**
   * An iteratee that will consume pairs of sentances and indexes and update a
   * specialized map used for counting
   */
  val concordanceIteratee: Iteratee[String, (Map[String, ItemCount[String]], Long)] = {
    def step(stateMap: Map[String, ItemCount[String]], index: Long)(s: Input[String]): Iteratee[String, (Map[String, ItemCount[String]], Long)] =
      s(
        empty = cont(step(stateMap,index)),
        el = sentance => {
          val newMap =
            sentance
              .split(' ')
              .map { w => alphaNumericRegex.replaceAllIn(w.trim.toLowerCase, "") }
              .filter { !_.isEmpty }
              .foldLeft(stateMap){ (acc, word) => acc.get(word) match {

                case None =>
                  /**
                   * this is the first time the word has been encountered.
                   */
                  acc.updated(word, ItemCount(word, 1, Map(index -> 1)))

                case Some(ItemCount(_, total, countMap)) =>
                  /**
                   * we have this this word before. Update total count and check
                   * if we've seen this in the sentance yet. 
                   */
                  countMap.get(index) match {

                    case None =>
                      /** this is the first time we've seen this word in the
                       * sentance
                       */
                      acc.updated(
                        word,
                        ItemCount(word, total+1,countMap.updated(index, 1))
                      )

                    case Some(sentanceCount) =>
                      /**
                       * we have seen this in a sentance before
                       */
                      acc.updated(
                        word,
                        ItemCount(word, total+1, countMap.updated(index, sentanceCount + 1))
                      )
                  }
              }}
          val newIndex = index + 1L
          cont(step(newMap, newIndex))
        },
        eof = done((stateMap,index), eofInput[String])
      )

    cont(step(Map.empty[String, ItemCount[String]], 0L))
  }

  val constantMemory = 
    concordanceIteratee.up[IO] %= 
      filter[String, IO]( !_.isEmpty) %= 
      sentanceIteratee.sequenceI &=
      fileReader

  def main(args: Array[String]) { 

    val text = "Hello. I am playing around with iteratees. Iteratees are fun."

    val naiveResult = text
      .split('.')
      .map { _.toLowerCase }
      .zipWithIndex
      .foldLeft(Map.empty[String, SentanceWordCount]){ case (accS, (sentance,index)) => 
        val lineCount: Map[String, Int] =
          sentance
            .split(' ')
            .filter { !_.isEmpty }
            .foldLeft(Map.empty[String, Int]){ (accW, word) =>
              accW.updated(word, accW.get(word).map { _ + 1 }.getOrElse { 1 })
            }

        lineCount.foldLeft(accS){ case (acc, (word, count)) => acc.get(word) match {
          case None => acc.updated(word, SentanceWordCount(word, count, (index, count) :: Nil))
          case i @ Some(SentanceWordCount(_, total, lineCounts)) =>
            acc.updated(word, SentanceWordCount(word, total + count, (index, count) :: lineCounts))
        }}
      }

      println("*** naive apporach:\n")

      naiveResult.foreach { case (_, b) => println(b) }

      println("*** using iteratees:\n")

      constantMemory
        .run
        .unsafePerformIO
        ._1
        .foreach { case (_, i) => println(i) }

  }


}
