package me.benetis

object Utils {

  def removeWord(word: String): Boolean = {
    val stopWords = List("the","a","http","i","me","to","what","in","", " ", "of", "and", "^").toSet
    stopWords.contains(word)
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


}
