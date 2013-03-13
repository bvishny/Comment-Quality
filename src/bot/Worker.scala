package bot

import bayes._
import scala.actors._
import scala.io.Source
import scala.xml._

class Worker(val devKey: String) extends Actor {
  var classifier: Bayes = new Bayes(List())
    
  def act() {
    	react {
    		// format (command, video key list, Bayes source file)
    		case (cmd: String) => if (cmd == "exit") exit() else if (cmd == "hb") { println(Actor.self); act() } else println("OK")
    		case (cmd: String, keyLst: List[String], bayesSource: String) => {
    			if (bayesSource != "") classifier.load(bayesSource) 
    			processKeys(keyLst)
    			act()
    		}
    	}
  }
    
  def processKeys(keyLst: List[String]) { 
    val endpoint = new xmlPOST("http://viralprints.com/api/submit_video_data?user_id=UID&user_secret=SECRET")
    println(endpoint.post(keyLst.foldLeft("") ((xml, k) => xml + processKey(k))))
  }
  
  def processKey(key: String) = {
    try {
    	val baseURI = "http://gdata.youtube.com/feeds/api/videos/"+ key + "?v=2&fields=yt:statistics&key=" + devKey
    	val viewCount = (XML.loadString(scala.io.Source.fromURL(baseURI).mkString) \\ "statistics") \ "@viewCount"
    	val commentURI = "http://gdata.youtube.com/feeds/api/videos/" + key + "/comments?v=2&start-index=1&max-results=50&key=" + devKey
    	val commentXML = XML.loadString(scala.io.Source.fromURL(commentURI).mkString)
    	val commentCount = (commentXML \\ "totalResults").text.toInt
    	val recentComments = (commentXML \\ "content").map(_.text)
    	val qualityScore = recentComments.foldLeft(0.0) { (sum, c) => 
    		val probs = classifier.probabilities(c)
    		Bayes ! ("train", List(if (probs(0)._2 >= probs(1)._2) "Relevant" else "Irrelevant", c), Actor.self)
    		sum + (probs(0)._2 / probs(1)._2)
    	} / recentComments.size.floatValue
    	"<v k='" + key + "'><d>" + List(commentCount, qualityScore, viewCount).mkString(":") + "</d></v>"
    } catch {
      case ioe: java.io.IOException => { // Catch bad request etc
        println(ioe.toString)
        "<v k='" + key + "'>null</v>" // signifies to server when submitted that the link si bad
      }
      case e: Exception => { // all other exceptions
        println(e.toString)
        "<v k='" + key + "'>na</v>" // signifies to server that no data is *currently* available
      }
    }
  }

}