package me.benetis

import cats.implicits._

/* https://en.wikipedia.org/wiki/Vector_space_model */

object VectorSpaceModelIndexer {

  case class DocumentWeights(weights: Vector[WeightForTerm], documentName: DocumentName, se: SE)
  case class WeightForTerm(term: String, weight: Double)
  case class VectorSpaceModelIndex(weights: Vector[DocumentWeights])
  case class VectorSpaceModelResults(docWithWeighs: DocumentWeights, similarity: Double)

  def apply(cleanTexts: Vector[CleanTextFromSearchEngine]): VectorSpaceModelIndex  = {
    val weights: Vector[DocumentWeights] = transformToWeights(cleanTexts)
    VectorSpaceModelIndex(weights)
  }

  private def getOrElse[T](xs: Vector[T], i: Int, orElse: T): T = {
    val x = xs.get(i)

    x match {
      case Some(res) => res
      case None => orElse
    }
  }

  def cosineSimilarity(document: DocumentWeights, query: Vector[WeightForTerm]): Double = {

    val topPartValue = document.weights
      .filterNot(_.weight == Double.NegativeInfinity)
      .zipWithIndex.foldLeft(0.0)((prev, curr: (WeightForTerm, Int)) => {
      prev + curr._1.weight * getOrElse(query.map(_.weight), curr._2, 0.0)
    })

    def sumSquareSqrt(xs: Vector[WeightForTerm]): Double = {
      Math.sqrt(xs.foldLeft(0.0)((prev, curr) => prev + Math.pow(curr.weight, 2)))
    }

    val bottomPart = sumSquareSqrt(document.weights) + sumSquareSqrt(query)

    topPartValue / bottomPart
  }

  def query(query: String, cleanTexts: Vector[CleanTextFromSearchEngine], index: VectorSpaceModelIndex): Vector[VectorSpaceModelResults] = {
    val mapOfFreq: Vector[FreqMapSE] = cleanTexts.map(LogicSpaceModelIndexer.textsToMapsOfFreq)
    val numberOfDocuments = cleanTexts.length

    val terms = query.split(" and ")

    val queryweights: Vector[WeightForTerm] = terms.map(term => {
      val n = numberOfDocumentsContainingTerm(term, mapOfFreq)

      weightForTermInDocument(
        term = term,
        frequencyInLocalDocument = 1,
        numberOfAllDocuments = numberOfDocuments,
        numberOfDocumentsContainingTerm = n
      )
    }).toVector

    println(s"Query: $query")

    index.weights.map(doc => VectorSpaceModelResults(doc, cosineSimilarity(doc, queryweights)))
      .sortWith((a, b) => a.similarity > b.similarity).take(5)
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

      val contains = if(curr.map.value.getOrElse(term, 0) >= 1)
        1
      else
        0

      prev + contains
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
