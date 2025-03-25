package jsonModels
import zio.json.JsonEncoder
import zio.json.DeriveJsonEncoder
import zio.json.ast.Json
import java.time.{Instant, Duration}

case class ItemsContainerJSON(map: Map[String, Int], level: Option[Int])
object ItemsContainerJSON {
  implicit val encoder: JsonEncoder[ItemsContainerJSON] = DeriveJsonEncoder.gen[ItemsContainerJSON]
}