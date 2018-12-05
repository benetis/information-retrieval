package me.benetis

import org.apache.commons.text.StringEscapeUtils

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
