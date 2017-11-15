import org.scalatest.FlatSpec

import scala.collection.immutable.ListMap

class WordFrequencyCounterTest extends FlatSpec {

  "count words" should "count a single word" in {
    val strings = Iterator("the")
    val wordCount = WordFrequencyCounter.countWords(strings)

    assert(wordCount.size === 1)
    assert(wordCount("the") === 1)
  }

  it should "count multiple of the same word" in {
    val strings = Iterator("the the the the the")
    val wordCount = WordFrequencyCounter.countWords(strings)

    assert(wordCount.size === 1)
    assert(wordCount("the") === 5)
  }

  it should "count ignoring case" in {
    val wordCount = WordFrequencyCounter.countWords(Iterator("the The tHe the the"))

    assert(wordCount.size === 1)
    assert(wordCount("the") === 5)
  }

  it should "count where multiple spaces between words" in {
    val wordCount = WordFrequencyCounter.countWords(Iterator("the the      the the the"))

    assert(wordCount.size === 1)
    assert(wordCount("the") === 5)
  }

  it should "count where new line between words" in {
    val wordCount = WordFrequencyCounter.countWords(Iterator("the the the \nthe the"))

    assert(wordCount.size === 1)
    assert(wordCount("the") === 5)
  }

  it should "count where tab between words" in {
    val wordCount = WordFrequencyCounter.countWords(Iterator("the\t the the the the"))

    assert(wordCount.size === 1)
    assert(wordCount("the") === 5)
  }

  it should "count multiple different words" in {
    val wordCount = WordFrequencyCounter.countWords(Iterator("the\t the man the man"))

    assert(wordCount.size === 2)
    assert(wordCount("the") === 3)
    assert(wordCount("man") === 2)
  }

  it should "not count empty words" in {
    val wordCount: ListMap[String, Int] = WordFrequencyCounter.countWords(Iterator(""))

    assert(wordCount.size === 0)
  }

  it should "order by count descending" in {
    val wordCount: ListMap[String, Int] = WordFrequencyCounter.countWords(Iterator("the\t the man the man had had had had had"))

    assert(wordCount.head == ("had" -> 5));
    assert(wordCount.tail.head == ("the" -> 3));
    assert(wordCount.tail.tail.head == ("man" -> 2));
  }
}
