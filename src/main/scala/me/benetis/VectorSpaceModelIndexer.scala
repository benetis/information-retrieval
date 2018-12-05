package me.benetis

import cats.implicits._

object VectorSpaceModelIndexer {

  case class


  def apply(cleanTexts: Vector[CleanTextFromSearchEngine]): VectorSpaceModelIndex  = {
    val mapOfFreq: Vector[FreqMapSE] = cleanTexts.map(textsToMapsOfFreq)
    VectorSpaceModelIndex(mapOfFreq.
      foldLeft(Map.empty[String, Int])((prev: Map[String, Int], curr: FreqMapSE) => prev.combine(curr.map)))
  }

  def euclidianNorm()

}
