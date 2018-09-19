package example
import java.nio.charset.CodingErrorAction
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._
import net.ruippeixotog.scalascraper.browser.{JsoupBrowser => JSB}
import scala.io.{Codec, Source}

sealed trait SE { val fileContents: Vector[Doc] }
case class Google(fileContents: Vector[Doc]) extends SE
case class Yandex(fileContents: Vector[Doc]) extends SE
case class Bing(fileContents: Vector[Doc]) extends SE
case class DuckDuckGo(fileContents: Vector[Doc]) extends SE
case class Yahoo(fileContents: Vector[Doc]) extends SE

case class Doc(body: String)

object Hello extends App {
//  Scraper()
  GetFiles().foreach(se => println(se.fileContents))
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


  def parseDocs(dirPath: String): Vector[Doc] = {
    getListOfFiles(dirPath).map(fileToString).map(Doc).toVector
  }

  def apply(): Vector[SE] = {
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
