import scala.collection.immutable.ListMap
import scala.io.Source

object WordFrequencyCounter extends App {

  // Testable function returning ordered data
  def countWords: (Iterator[String]) => ListMap[String, Int] =
    allLines => {
      ListMap.empty ++
        allLines.flatMap(line => line.split("\\W+"))
          .map(word => word.toLowerCase)
          .filter(word => !word.isEmpty)
          .toStream
          .groupBy(word => word)
          .mapValues(_.length)
          .toStream
          .sortBy(kvTuple => -kvTuple._2)
    }

  val linesIterator = Source.fromFile("moby_dick_no_punctuation.txt").getLines()

  // Run it
  countWords(linesIterator)
    .foreach(wordCountTuple => println(s"${wordCountTuple._1},${wordCountTuple._2}"))
}