package jsonModels

import zio.json.JsonEncoder
import zio.json.DeriveJsonEncoder
import zio.json.ast.Json
import java.time.{Instant, Duration}
import gameClasses.Order

case class OrderJSON(orderItems: Map[String, Int], orderPrice: Int, orderTokenPrice: Int, completed: Boolean, timeStamp: Long)
object OrderJSON {
  implicit val encoder: JsonEncoder[OrderJSON] = DeriveJsonEncoder.gen[OrderJSON]
}
