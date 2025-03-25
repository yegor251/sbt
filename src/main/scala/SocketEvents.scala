import zio._

sealed trait SocketEvent


object SocketEvent {
  case class Connect(data: Option[String])                  extends SocketEvent
  case class Use(data: (String, Int, Int))                  extends SocketEvent
  case class Collect(data: (Int, Int))                      extends SocketEvent
  case class Upgrade(data: (Int, Int))                      extends SocketEvent
  case class Buy(data: (String, Int))                       extends SocketEvent
  case class Place(data: (String, Int, Int))                extends SocketEvent
  case class Move(data: (Int, Int, Int, Int))               extends SocketEvent
  case class SpinWheel(data: String)                        extends SocketEvent
  case class OrderOperation(data: (String, Int))            extends SocketEvent
  case class Regeneration(data: String)                     extends SocketEvent
  case class ClaimDeposit(data: Int)                        extends SocketEvent
  case class RegisterWithdraw(data: (Long, String))         extends SocketEvent
  case class PurchaseSlot(data: (Int, Int))                 extends SocketEvent
  case class PurchaseDeal(data: String)                     extends SocketEvent
  case class ActivateBooster(data: Int)                     extends SocketEvent
  case class InvUpgrade(data: String)             	        extends SocketEvent
  case class BusinessEV(data: (String, Int, Int))           extends SocketEvent
  case class RemoveObstacle(data: (Int, Int))               extends SocketEvent
  case class Unknown(data: String)                          extends SocketEvent
}
import SocketEvent._


def stringToEventZIO(message: String): UIO[SocketEvent] = ZIO.succeed {
  println(message)
  message match {
    case (s"connect/${text}") => try                Connect(WebAppSignatureValidator.checkWebAppSignature(WebAppSignatureValidator.token, text))    catch _ => Unknown(null)
    case (s"use/${name}/${x}/${y}") => try          Use(Tuple3(name, x.toInt, y.toInt))                             catch _ => Unknown(null)
    case (s"collect/${x}/${y}") => try              Collect(Tuple2(x.toInt, y.toInt))                               catch _ => Unknown(null)
    case (s"upgrade/${x}/${y}") => try              Upgrade(Tuple2(x.toInt, y.toInt))                               catch _ => Unknown(null)
    case (s"buy/${name}/${amount}") => try          Buy(Tuple2(name, amount.toInt))                                 catch _ => Unknown(null)
    case (s"spin") => try                           SpinWheel(null)                                                 catch _ => Unknown(null)
    case (s"regen") => try                          Regeneration(null)                                              catch _ => Unknown(null)
    case (s"order/${operation}/${index}") => try    OrderOperation(Tuple2(operation, index.toInt))                  catch _ => Unknown(null)
    case (s"place/${name}/${x}/${y}") => try        Place(Tuple3(name, x.toInt, y.toInt))                           catch _ => Unknown(null)
    case (s"move/${x}/${y}/${to_x}/${to_y}") => try Move(Tuple4(x.toInt, y.toInt, to_x.toInt, to_y.toInt))          catch _ => Unknown(null)
    case (s"claim/${index}") => try                 ClaimDeposit(index.toInt)                                       catch _ => Unknown(null)
    case (s"withdraw/${amount}/${wallet}") => try   RegisterWithdraw(Tuple2(amount.toLong, wallet))                 catch _ => Unknown(null)
    case (s"buyslot/${x}/${y}") => try              PurchaseSlot(Tuple2(x.toInt, y.toInt))                          catch _ => Unknown(null)
    case (s"buydeal/${name}") => try                PurchaseDeal(name)                                              catch _ => Unknown(null)
    case (s"activateb/${index}") => try             ActivateBooster(index.toInt)                                    catch _ => Unknown(null)
    case (s"invupgrade") => try             				InvUpgrade(null)                                    						catch _ => Unknown(null)
    case (s"business/${event}/${id}/${i}") => try   BusinessEV(Tuple3(event, id.toInt, i.toInt))                    catch _ => Unknown(null)
    case (s"rmobstacle/${x}/${y}") => try           RemoveObstacle(Tuple2(x.toInt, y.toInt))                               catch _ => Unknown(null)
    case _ => Unknown(null);
}}