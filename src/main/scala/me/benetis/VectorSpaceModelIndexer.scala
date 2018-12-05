package me.benetis

import cats.implicits._

object VectorSpaceModelIndexer {

  case class DocumentWeights(weights: Vector[WeightForTerm], documentName: DocumentName, se: SE)
  case class WeightForTerm(term: String, weight: Double)
  case class VectorSpaceModelIndex(weights: Vector[DocumentWeights])

  def apply(cleanTexts: Vector[CleanTextFromSearchEngine]): VectorSpaceModelIndex  = {
    val weights: Vector[DocumentWeights] = transformToWeights(cleanTexts)
    println(weights)
    VectorSpaceModelIndex(weights)
  }

  def transformToWeights(
        cleanTexts: Vector[CleanTextFromSearchEngine]
  ): Vector[DocumentWeights] = {

    val mapOfFreq: Vector[FreqMapSE] = cleanTexts.map(LogicSpaceModelIndexer.textsToMapsOfFreq)

    val numberOfDocuments = cleanTexts.length

    mapOfFreq.map(onefreqMapSe => {
      val allDocumentTerms: Iterable[String] = onefreqMapSe.map.value.keys
      DocumentWeights(
        allDocumentTerms.map(t =>
        weightForTermInDocument(
          t,
          onefreqMapSe.map.value.getOrElse(t, 0),
          numberOfDocuments,
          numberOfDocumentsContainingTerm(t, mapOfFreq)
        )).toVector,
        onefreqMapSe.documentName, onefreqMapSe.se)
    })
  }

  def numberOfDocumentsContainingTerm(term: String, mapsOfFreq: Vector[FreqMapSE]): Int = {
    mapsOfFreq.foldLeft(0)((prev: Int, curr: FreqMapSE) => {
      prev + curr.map.value.getOrElse(term, 0)
    })
  }

  def weightForTermInDocument(term: String, frequencyInLocalDocument: Int,
                              numberOfAllDocuments: Int,
                              numberOfDocumentsContainingTerm: Int): WeightForTerm = {
    val weight = frequencyInLocalDocument * inverseDocumentFrequency(
      numberOfAllDocuments = numberOfAllDocuments,
      numberOfDocumentsContainingTerm = numberOfDocumentsContainingTerm
    )

    WeightForTerm(term, weight)
  }

  def inverseDocumentFrequency(numberOfAllDocuments: Int, numberOfDocumentsContainingTerm: Int): Double = {
    Math.log(numberOfAllDocuments / numberOfDocumentsContainingTerm)
  }

}
