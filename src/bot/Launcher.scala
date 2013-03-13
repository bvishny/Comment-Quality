package bot

import bayes._
import scala.actors._
import scala.io.Source
import collection.mutable.HashMap

object Launcher {

  var workers: List[Worker] = List[Worker]()
  val sourcePath = "src/support/baye.obj"
  val devKeys = List("DEVKEY")
  val runInterval = 7200000 // time in ms to sleep
  
  def main(args: Array[String]): Unit = {
	run()
  }
  
  def run() {
    val vidURI = "http://viralprints.com/api/list_videos?user_id=UID&user_secret=SECRET"
    val vidData = scala.io.Source.fromURL(vidURI).mkString.split(",") 
    val numWorkers = vidData(0).toInt // Number of actors
    var vidKeys = vidData.slice(1, vidData.size - 1).toBuffer // List of YouTube video keys
    val perWorker = vidKeys.size / numWorkers
    val workerAssignments = collection.mutable.Map() ++ ((1 to numWorkers).map { 
      (i) => (i, (1 to (perWorker + (if (i == numWorkers) (vidKeys.size % numWorkers) else 0))).foldLeft(List[String]()) { 
        (lst, j) => vidKeys.remove(0) +: lst
      }) 
    }).toMap
    // Initialize Global Classifier
    Bayes.start()
    // Load saved settings
    Bayes ! ("load", List(sourcePath), Actor.self)
    // Random number generator 
    val rand = new scala.util.Random
    // Initialize Workers, starting new ones if worker count has increased
    (1 to (numWorkers - workers.size)).foreach((x) => {
      val wrk = new Worker(devKeys(rand.nextInt(devKeys.size))) 
      // If multiple YTB dev keys are available, randomly assign to workers to prevent high volume block
      wrk.start()
      workers :+= wrk
    })
    // Assign videos to each worker
    workerAssignments.keys.foreach((k) => workers(k - 1) ! ("run", workerAssignments(k), sourcePath))
    // Sleep then run again
    println("Restart again in: " + runInterval.toString)
    Thread.sleep(runInterval)
    run()
  }

}