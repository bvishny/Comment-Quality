package bayes

import scala.io.Source
import collection.mutable.HashMap

// For Serialization
import java.io.ObjectInputStream
import java.io.FileInputStream
import java.io.ObjectOutputStream
import java.io.FileOutputStream

class Bayes(var categories: List[String], 
    var sourceFile: String = null,
    val parser: WordParser = new WordParser())
    extends Serializable {
  
   private var catDocs: HashMap[String, Int] = new HashMap()
   private var catWords: HashMap[Pair[String, String], Int] = new HashMap()
   private var catWordTotals: HashMap[String, Int] = new HashMap()
   
   // Comment out the below section unless testing
   def _catDocs() : HashMap[String, Int] = { catDocs }
   def _catWords() : HashMap[Pair[String, String], Int] = { catWords }
   def _catWordTotals() : HashMap[String, Int] = { catWordTotals }
   
   def train(cat: String, doc: String) : Bayes =  {
     catDocs.put(cat, catDocs.getOrElse(cat, 0) + 1) // Increment category doc count
     parser.parse(doc).wordIndex.foreach(item => catWords.put(Pair(cat, item._1), 
         catWords.getOrElse((cat, item._1), 0) + item._2)) // Insert new words
     calcWordTotal(cat) // Update category total word count
     this
   }
   
   def probabilities(doc: String) : List[(String, Double)] = {
     categories.map((cat) => (cat, prCatIfDoc(cat, doc.toLowerCase())))
   }
   
   def classify(doc: String) : (String, Double) = { // (Category, Probability)
     probabilities(doc).maxBy(_._2)
   }
   
   def save(filePath: String) : Unit = {
      val out = new ObjectOutputStream(new FileOutputStream(filePath))
      out.writeObject(HashMap("catDocs" -> catDocs, "catWords" -> catWords, "catWordTotals" -> catWordTotals)) 
      out.close()
   }

   def load(filePath: String) : Bayes = {
     val hash = new ObjectInputStream(new FileInputStream(filePath)).readObject().asInstanceOf[HashMap[String,AnyRef]]
     catDocs = hash("catDocs").asInstanceOf[HashMap[String, Int]]
     catWords = hash("catWords").asInstanceOf[HashMap[(String, String), Int]]
     catWordTotals = hash("catWordTotals").asInstanceOf[HashMap[String,Int]]
     categories = catDocs.keys.toList
     sourceFile = filePath
     this
   }
   
   private 
   
   def prCatIfDoc(cat: String, doc: String) : Double = { // Pr(C|D)
     prCat(cat) * parser.parse(doc).stemmedList.foldLeft(1.0) ((product, word) =>  product * prWordIfCat(word, cat))
   }
   
   def prCat(cat: String) : Double = { // Pr(C)
     (catDocs.getOrElse(cat, 0).floatValue) /  (catDocs.values.foldLeft(0) (_+_))
   }

  
   def prWordIfCat(word: String, cat: String) : Double = { // Pr(W|C)
     // Divide by 0 error if catWordTotals not properly populated
     (catWords.getOrElse(Pair(cat, word), 0).floatValue + 1.0) / 
     	catWordTotals.getOrElse(cat, 0).floatValue
   }
   
   def calcWordTotal(cat: String) : Int = {
     val newTotal = (catWords.filter((word) => word._1._1 == cat).values.foldLeft(0) 
         ((sum, count) => (sum + count)))
     catWordTotals.put(cat, newTotal)
     newTotal
   }
   
}

import scala.actors._

object Bayes extends Actor {
  
  def act() {
     actProxy(new Bayes(List()))
  }
  
  def actProxy(instance: Bayes, saveAfter: Int = 3000) {
     react {
      case (cmd: String, params: List[String], act: Actor) => cmd match {
        case "load" => actProxy(instance.load(params(0)))
        case "save" => instance.save(params(0)); actProxy(instance)
        case "exit" => {
          instance.save(params(0))
          act ! ("bayes", "main Bayes exited")
          exit()
        }
        case "train" => {
          if (saveAfter == 0) { // BackUp classifications every 3000 trains
            println("SAVING TO " + instance.sourceFile)
            instance.save(instance.sourceFile)
          }
          actProxy(instance.train(params(0), params(1)), 
              if (saveAfter == 0) 3000 else (saveAfter - 1))
        }
        case "classify" => {
          act ! instance.classify(params(0))
          actProxy(instance, saveAfter)
        }
      }
    }
  }
}