package example
import java.nio.charset.CodingErrorAction
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._
import net.ruippeixotog.scalascraper.browser.{JsoupBrowser => JSB}
import scala.io.{Codec, Source}
import edu.stanford.nlp.simple._
import org.apache.commons.text.StringEscapeUtils
import scala.collection.immutable
import scala.collection.immutable.ListMap
import cats._
import cats.data._
import cats.implicits._
import cats.Semigroup

sealed trait SE { val dirPath: String }
case class Google(dirPath: String) extends SE
case class Yandex(dirPath: String) extends SE
case class Bing(dirPath: String) extends SE
case class DuckDuckGo(dirPath: String) extends SE
case class Yahoo(dirPath: String) extends SE

sealed trait Doc { val text: String }
case class HTMLText(text: String) extends Doc
case class CleanText(text: String) extends Doc

case class TextFromSearchEngine(se: SE, doc: Doc)

case class FreqMapSE(se: SE, map: Map[String, Int])
case class SortedFreqMapSE(se: SE, map: ListMap[String, Int])

case class Index(map: Map[String, Int])

object ParsingHeaven extends App {
  val searchEngineList = Vector(
    Google("google"),
    Yandex("yandex"),
    DuckDuckGo("duckduckgo"),
    Yahoo("yahoo"),
    Bing("bing")
  )
  val textsFromSearchEngines: Vector[TextFromSearchEngine] = GetFiles(searchEngineList)
  val cleanTexts: Vector[TextFromSearchEngine] = textsFromSearchEngines.map(t => t.copy(doc = Html2PlainText(t.doc)))

  val builtIndex: Index = Indexer(cleanTexts)

  println(builtIndex.map)

}

object Search {
  def apply(index: Index): Vector[String] = {
    Vector()
  }
}

object Indexer {

  def apply(cleanTexts: Vector[TextFromSearchEngine]): Index  = {
    val mapOfFreq: Vector[FreqMapSE] = cleanTexts.map(textsToMapsOfFreq)
    Index(mapOfFreq.foldLeft(Map.empty[String, Int])((prev: Map[String, Int], curr: FreqMapSE) => prev.combine(curr.map)))
  }

  def sortedIndex(mapOfFreq: Vector[FreqMapSE]): Vector[SortedFreqMapSE] = {
    mapOfFreq.map(sortByFreq)
  }

  private def textsToMapsOfFreq(cleanText: TextFromSearchEngine): FreqMapSE = {
    val words = cleanText.doc.text.split(" ").map(_.replaceAll("\\s", ""))

      FreqMapSE(
        cleanText.se,
        words.map(word =>(word, 1))
      .groupBy(_._1).mapValues(_.map(_._2).sum)
          .filterNot(t => removeWord(t._1))
      )
  }

  private def removeWord(word: String): Boolean = {
    val stopWords = List("the","a","http","i","me","to","what","in","", " ", "of", "and", "^").toSet
    stopWords.contains(word)
  }

  private def sortByFreq(mapOfFreq: FreqMapSE): SortedFreqMapSE = {
    SortedFreqMapSE(mapOfFreq.se, ListMap(mapOfFreq.map.toSeq.sortWith(_._2 > _._2):_*))
  }
}

object Html2PlainText {

  /* https://cindyxiaoxiaoli.wordpress.com/2014/02/05/html-to-plain-text-with-java/ */

  import org.jsoup.Jsoup
  import org.jsoup.nodes.Document
  import org.jsoup.safety.Whitelist

  def apply(html: Doc): CleanText = {
    val plainTextPipeline = cleanTagPerservingLineBreaks _ andThen unescapeHTML andThen removeUrl andThen additionalCleaning andThen removeExtendedChars

    CleanText(plainTextPipeline(html.text))
  }

  private def cleanTagPerservingLineBreaks(html: String): String = {
    var result = ""
    if (html == null) return html
    val document = Jsoup.parse(html)
    document.outputSettings(new Document.OutputSettings().prettyPrint(false))// makes html() preserve linebreaks and

    document.select("br").append("\\n")
    document.select("p").prepend("\\n\\n")
    result = document.html.replaceAll("\\\\n", "\n")
    result = Jsoup.clean(result, "", Whitelist.none, new Document.OutputSettings().prettyPrint(false))
    result
  }

  private def unescapeHTML(str: String): String = StringEscapeUtils.unescapeHtml4(str)

  private def removeUrl(str: String): String = {
    val regex = "\\b(https?|ftp|file|telnet|http|Unsure)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]"
    str.replaceAll(regex, "")
  }

  private def additionalCleaning(str: String): String = {
    val lowerCase = str.toLowerCase()
    lowerCase.replaceAll(":", "")
  }

  private def removeExtendedChars(str: String): String = str.replaceAll("[^\\x00-\\x7F]", " ")

}


object GetFiles {
  import java.io.File

  def getListOfFiles(dir: String): List[String] = {
    val file = new File(dir)
    file.listFiles.filter(_.isFile)
      .map(_.getPath).toList
  }

  def fileToString(filePath: String): String = {
    //https://stackoverflow.com/questions/13625024/how-to-read-a-text-file-with-mixed-encodings-in-scala-or-java
    val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)
    Source.fromFile(filePath)(decoder).mkString
  }


  def parseDocs(dirPath: String, searchEngine: SE): Vector[TextFromSearchEngine] = {
    getListOfFiles(dirPath).map(fileToString)
      .map(text => TextFromSearchEngine(searchEngine, HTMLText(text))).toVector
  }

  def apply(searchEngineList: Vector[SE]): Vector[TextFromSearchEngine] = {
    searchEngineList.flatMap((se: SE) => parseDocs(getClass.getResource(s"/${se.dirPath}").getPath, se))
  }
}

//object Scraper {
//  def apply() = {
//    val browser = new JsoupBrowser("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36")
//
//    val duckduckGo = browser.get("https://duckduckgo.com/?q=leaving+european+union&t=h_&ia=web")
//
//    println(duckduckGo.body)
//
//    val results = duckduckGo >> elementList("h2 > a.result__a")
//
//    println(results)
//  }
//}
//
