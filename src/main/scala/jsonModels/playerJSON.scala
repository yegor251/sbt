package jsonModels
import _root_.gameClasses._
import zio.json.JsonEncoder
import zio.json.DeriveJsonEncoder
import zio.json.ast.Json
import java.time.{Instant, Duration}

case class PlayerJSON(money: Int, networth: Int, Inventory: ItemsContainerJSON, spin: Option[SpinJSON], orders: Option[Array[OrderJSON]])
object PlayerJSON {
  def fromPlayer(player: Player): PlayerJSON =
  {
    PlayerJSON(
            player.money, 
            player.netWorth,
            ItemsContainerJSON(player.Inventory.items, Some(player.Inventory.level)),
            if (player.spin == null) None else 
              Some(SpinJSON(player.spin.items.map { case (item, count) => SlotJSON(item, count)}, SlotJSON(player.spin.drop._1, player.spin.drop._2), player.spin.generateTimeStamp.getEpochSecond(), player.spin.activated)),
            if (player.orders == null) None else
              Some(player.orders.map(order => OrderJSON(order.items, order.price, order.tokenPrice, order.completed, order.startTimeStamp.getEpochSecond())))
          )
  }
  implicit val encoder: JsonEncoder[PlayerJSON] = DeriveJsonEncoder.gen[PlayerJSON]
}