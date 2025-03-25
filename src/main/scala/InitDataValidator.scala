import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.util.Base64
import _root_.`<empty>`.ENVars

object WebAppSignatureValidator {
  
  val token = "7734943379:AAGF2_IbH6ZgMqU9T5o-WOzQsBLbjTWR4xU"

  def parseInitData(data: String): Map[String, String] = {
    data.split("&").flatMap { pair =>
      pair.split("=").toList match {
        case key :: value :: Nil => Some(URLDecoder.decode(key, "UTF-8") -> URLDecoder.decode(value, "UTF-8"))
        case _ => None
      }
    }.toMap
  }

  def hmacSHA256(key: Array[Byte], data: String): Array[Byte] = {
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(key, "HmacSHA256"))
    mac.doFinal(data.getBytes("UTF-8"))
  }

  def checkWebAppSignature(token: String, initData: String): Option[String] = {
    println(initData)
    parseInitData(initData) match {
      case parsedData if parsedData.contains("hash") =>
        val hash = parsedData("hash")
        val dataCheckString = parsedData
          .filterKeys(_ != "hash")
          .toSeq
          .sortBy(_._1)
          .map { case (k, v) => s"$k=$v" }
          .mkString("\n")
        
        val secretKey = hmacSHA256("WebAppData".getBytes("UTF-8"), token)
        val calculatedHash = hmacSHA256(secretKey, dataCheckString)
        val calculatedHashHex = calculatedHash.map("%02x".format(_)).mkString
        val user = parsedData("user").stripPrefix("{")
          .stripSuffix("}")
          .split(",")
          .map { pair =>
            val Array(key, value) = pair.split(":", 2).map(_.trim)
            val cleanKey = key.stripPrefix("\"").stripSuffix("\"")
            val cleanValue = value.stripPrefix("\"").stripSuffix("\"")
            cleanKey -> cleanValue
          }.toMap
        if (calculatedHashHex == hash) Some(s"${user("id")}") else None

      case _ => None
    }
  }

}
