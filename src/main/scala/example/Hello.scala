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

sealed trait SE[+T] { val fileContents: Vector[T] }
case class Google[T](fileContents: Vector[T]) extends SE[T]
case class Yandex[T](fileContents: Vector[T]) extends SE[T]
case class Bing[T](fileContents: Vector[T]) extends SE[T]
case class DuckDuckGo[T](fileContents: Vector[T]) extends SE[T]
case class Yahoo[T](fileContents: Vector[T]) extends SE[T]

sealed trait Doc { val text: String }
case class HTMLText(text: String) extends Doc
case class CleanText(text: String) extends Doc

object Hello extends App {
  val textsFromSearchEngines: Vector[SE[HTMLText]] = GetFiles()
  val cleanTexts = textsFromSearchEngines.map(_.fileContents.map(Html2PlainText(_)))
  cleanTexts.foreach(println)
}

object Html2PlainText {

  /* https://cindyxiaoxiaoli.wordpress.com/2014/02/05/html-to-plain-text-with-java/ */

  import org.jsoup.Jsoup
  import org.jsoup.nodes.Document
  import org.jsoup.safety.Whitelist

  def apply(html: HTMLText): CleanText = {
    val plainTextPipeline = cleanTagPerservingLineBreaks _ andThen unescapeHTML andThen removeUrl andThen removeExtendedChars

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


  def parseDocs(dirPath: String): Vector[HTMLText] = {
    getListOfFiles(dirPath).map(fileToString).map(HTMLText).toVector
  }

  def apply(): Vector[SE[HTMLText]] = {
    Vector(
      Bing(parseDocs(getClass.getResource("/bing").getPath)),
      Google(parseDocs(getClass.getResource("/google").getPath)),
      DuckDuckGo(parseDocs(getClass.getResource("/duckduckgo").getPath)),
      Yahoo(parseDocs(getClass.getResource("/yahoo").getPath)),
      Yandex(parseDocs(getClass.getResource("/yandex").getPath))
    )
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
