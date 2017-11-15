import scala.collection.immutable.ListMap
import scala.io.Source

object WordFrequencyCounter extends App {

  def splitIntoWords = (line: String) => line.split("\\W+").toList

  def lowerCase = (word: String) => word.toLowerCase

  def nonEmptyWords = (word: String) => !word.isEmpty;

  def word = (word: String) => word

  def count = (words: Stream[String]) => words.length

  def wordCount = (wordCountTuple: (String, Int)) => -wordCountTuple._2

  def countWords: (Iterator[String]) => ListMap[String, Int] =
    allLines => {
      ListMap.empty ++
        allLines.flatMap(splitIntoWords)
          .map(lowerCase)
          .filter(nonEmptyWords)
          .toStream
          .groupBy(word)
          .mapValues(count)
          .toStream
          .sortBy(wordCount)
    }

  // Run it
  val linesIterator = Source.fromFile("moby_dick_no_punctuation.txt").getLines()

  countWords(linesIterator)
    .foreach(wordCountTuple => println(s"${wordCountTuple._1},${wordCountTuple._2}"))
}