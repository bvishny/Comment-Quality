package bayes

import scala.io.Source
import collection.mutable.HashMap


class WordParser(val uselessWords:Set[String]=
  Source.fromFile("src/support/useless_words.txt").getLines().toSet,
  val filterRegex:List[String]=List("""[^a-zA-Z\s]+""", """['`]"""))
  extends Serializable {
  
  var wordIndex: HashMap[String, Int] = new HashMap()
  var stemmedList: List[String] = List()
  val stemmer = new snowball.ext.englishStemmer()
  
  def stemWrapper(ls: List[String]) : List[String] = {
    ls.map{(word) => stemmer.setCurrent(word); stemmer.stem(); stemmer.getCurrent()}
  }
  
  def incrementWord(word: String) : (String, Int) = {
    var newIdx = 0
    wordIndex.put(word, {newIdx = 1 + wordIndex.getOrElse(word, 0); newIdx})
    (word, newIdx)
  }
  
  def parse(input: String) : WordParser = {
    var text = input.toLowerCase()
    wordIndex = new HashMap() // Reset index
	filterRegex.foreach((reg) => text = text.replaceAll(reg, ""))
	stemmedList = stemWrapper(text.split("""\s+""").toList.filter({
	  (word) => if (!uselessWords.contains(word)) { 
	    incrementWord(word); true 
	  } else { 
	    false 
	  } 
	}))
	this
  }
}

