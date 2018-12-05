package me.benetis

import java.nio.charset.CodingErrorAction
import scala.io.{Codec, Source}

object GetFiles {
  import java.io.File

  var fileName = 1

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

  def parseDocs(dirPath: String, searchEngine: SE): Vector[DirtyTextFromSearchEngine] = {
    getListOfFiles(dirPath).map(fileToString)
      .map(text => DirtyTextFromSearchEngine(searchEngine, HTMLText(text), DocumentName(MD5.hash(text)))).toVector
  }

  def apply(searchEngineList: Vector[SE]): Vector[DirtyTextFromSearchEngine] = {
    searchEngineList.flatMap((se: SE) => parseDocs(getClass.getResource(s"/${se.dirPath}").getPath, se))
  }
}
