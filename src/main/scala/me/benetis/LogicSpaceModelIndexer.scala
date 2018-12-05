package me.benetis

import edu.stanford.nlp.simple.{Document, Sentence}
import java.util
import scala.collection.immutable.ListMap
import scala.collection.JavaConversions._
import cats.implicits._

object LogicSpaceModelIndexer {

  def apply(cleanTexts: Vector[CleanTextFromSearchEngine]): LogicModelIndex  = {
    val mapOfFreq: Vector[FreqMapSE] = cleanTexts.map(textsToMapsOfFreq)
    LogicModelIndex(
      DocumentTermsFrequency(mapOfFreq.foldLeft(Map.empty[String, Int])((prev: Map[String, Int], curr: FreqMapSE) => prev.combine(curr.map.value))))
  }

  def sortedIndex(cleanTexts: Vector[CleanTextFromSearchEngine]): Vector[SortedFreqMapSE] = {
    val mapOfFreq: Vector[FreqMapSE] = cleanTexts.map(textsToMapsOfFreq)
    mapOfFreq.map(sortByFreq)
  }

  implicit class UtilList(list: util.List[Sentence]) {
    def map[A, B](f: Sentence => B): Vector[B] = {
      var newList = scala.collection.mutable.ListBuffer.empty[B]
      list.forEach(sent => {
        newList += f(sent)
      })
      newList.toVector
    }
  }

  def textsToMapsOfFreq(cleanText: CleanTextFromSearchEngine): FreqMapSE = {

    val doc: Document = new Document(cleanText.doc.text)

    val sentences: util.List[Sentence] = doc.sentences()

    val lemmas: Vector[util.List[String]] = sentences.map((s: Sentence) => s.lemmas())

    val lemmasConcat: Vector[String] = lemmas.foldLeft(Vector.empty[String])((prev, curr: util.List[String]) => {
      prev ++ curr
    })

      FreqMapSE(
        cleanText.se,
        DocumentTermsFrequency(
          lemmasConcat.map(word =>(word, 1))
      .groupBy(_._1).mapValues(_.map(_._2).sum)
          .filterNot(t => Utils.removeWord(t._1))
        ),
        cleanText.documentName
      )
  }


  private def sortByFreq(mapOfFreq: FreqMapSE): SortedFreqMapSE = {
    SortedFreqMapSE(mapOfFreq.se, ListMap(mapOfFreq.map.value.toSeq.sortWith(_._2 > _._2):_*), mapOfFreq.documentName)
  }
}
