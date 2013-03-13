package bot
import scala.io.Source
import java.net._
import scala.xml._

class xmlPOST(val uri: String) {

	def post(xml: String) : String = {
		val url = new java.net.URL(uri)
        val out = ("<?xml version='1.0' encoding='utf-8'?><envelope>" + xml + "</envelope>").getBytes
        val conn = url.openConnection.asInstanceOf[java.net.HttpURLConnection]
        try {
            conn.setRequestMethod("POST")
            conn.setDoOutput(true)
            conn.setRequestProperty("Content-Length", out.length.toString)
            conn.setRequestProperty("Content-Type", "text/xml")
            conn.getOutputStream.write(out)
            conn.getOutputStream.close
            io.Source.fromInputStream(conn.getInputStream).mkString("")
        }
        catch {
            case e: Exception => scala.io.Source.fromInputStream(conn.getErrorStream).mkString("")
        }
     }
	
}