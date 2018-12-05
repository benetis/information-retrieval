package me.benetis

import scala.collection.immutable.ListMap
import cats.effect.IO
import me.benetis.VectorSpaceModelIndexer.DocumentWeights

sealed trait SE { val dirPath: String }
case class Google(dirPath: String) extends SE
case class Yandex(dirPath: String) extends SE
case class Bing(dirPath: String) extends SE
case class DuckDuckGo(dirPath: String) extends SE
case class Yahoo(dirPath: String) extends SE

sealed trait Doc { val text: String }
case class HTMLText(text: String) extends Doc
case class CleanText(text: String) extends Doc

case class DocumentName(name: String)
case class DirtyTextFromSearchEngine(se: SE, doc: Doc, documentName: DocumentName)
case class CleanTextFromSearchEngine(se: SE, doc: Doc, documentName: DocumentName)

case class DocumentTermsFrequency(value: Map[String, Int])

case class FreqMapSE(se: SE, map: DocumentTermsFrequency, documentName: DocumentName)
case class SortedFreqMapSE(se: SE, map: ListMap[String, Int], documentName: DocumentName)

case class DocumentWithSE(documentName: DocumentName, se: SE)

case class LogicModelIndex(map: DocumentTermsFrequency)


object ParsingHeaven extends App {
  val searchEngineList = Vector(
    Google("google"),
    Yandex("yandex"),
    DuckDuckGo("duckduckgo"),
    Yahoo("yahoo"),
    Bing("bing")
  )
  val textsFromSearchEngines: Vector[DirtyTextFromSearchEngine] = GetFiles(searchEngineList)
  val cleanTexts: Vector[CleanTextFromSearchEngine] = textsFromSearchEngines
    .map(t => CleanTextFromSearchEngine(
      se = t.se,
      documentName = t.documentName,
      doc = Html2PlainText(t.doc))
    )

//  LogicModelRunner.run(cleanTexts).unsafeRunSync()
  VectorSpaceModelRunner.run(cleanTexts).unsafeRunSync()

}

object VectorSpaceModelRunner {
  def run(cleanTexts: Vector[CleanTextFromSearchEngine]): IO[Unit] = {
    IO {
      VectorSpaceModelIndexer(cleanTexts)
    }
  }
}

object LogicModelRunner {
  def run(cleanTexts: Vector[CleanTextFromSearchEngine]): IO[Unit] = {
    IO {
      val builtIndex: LogicModelIndex = LogicSpaceModelIndexer(cleanTexts)
      println("Full index, merged from all documents")
      println(builtIndex.map)

      val documentsIndexes = LogicSpaceModelIndexer.sortedIndex(cleanTexts)

      println("All document indexes")
      documentsIndexes.foreach(i => println(i.documentName))

      println("Metaindexer with query and 'AND' support")
      MetaIndexer("european and sweden", documentsIndexes).foreach(println)

    }
  }
}


