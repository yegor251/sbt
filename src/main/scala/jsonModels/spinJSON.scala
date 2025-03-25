package jsonModels
import zio.json.JsonEncoder
import zio.json.DeriveJsonEncoder
import zio.json.ast.Json
import java.time.{Instant, Duration}

case class SlotJSON(item: String, amount: Int)
object SlotJSON {
  implicit val encoder: JsonEncoder[SlotJSON] = DeriveJsonEncoder.gen[SlotJSON]
}

case class SpinJSON(items: Array[SlotJSON], drop: SlotJSON, generateTimeStamp: Long, activated: Boolean)
object SpinJSON {
  implicit val encoder: JsonEncoder[SpinJSON] = DeriveJsonEncoder.gen[SpinJSON]
}