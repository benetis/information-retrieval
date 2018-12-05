package me.benetis

object LogicModelMetaIndexer {

  case class Query(terms: Array[String])

  def apply(query: String, documentIndexes: Vector[SortedFreqMapSE]): Vector[DocumentWithSE] = {
    filterIndexes(splitQuery(query), documentIndexes)
  }

  private def splitQuery(query: String): Query = {
    Query(query.split(" and "))
  }

  private def filterIndexes(query: Query, documentIndexes: Vector[SortedFreqMapSE]): Vector[DocumentWithSE] = {
    documentIndexes.flatMap(index => {
      val documentKeywords = index.map.keySet
      val queryKeywordsByAnd = query.terms.toSet

      if (queryKeywordsByAnd.subsetOf(documentKeywords))
        Some(DocumentWithSE(index.documentName, index.se))
      else None
    })
  }
}
