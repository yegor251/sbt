package gameClasses

import java.time.Instant
import Boosters._

class Player extends Serializable{
  object Inventory extends ItemsContainer;
  var money = 20;
  var netWorth = 0;
  var spin: Spin = Spin(Array(("money", 10), ("bread", 1), ("wheat", 1), ("wheat", 2), ("wheat", 3), ("wheat", 4)), ("wheat", 2));
  var orders: Array[Order] = Array(Order(Map("bread" -> 1), 15, Instant.now(), 0));

  object Businesses extends Serializable
  {
    var businesses: Array[Option[Business]] = Array.fill(BusinessManager.businessSettings.amount)(None)
  }

  object Stats extends Serializable
  {
    var ordersCompleted: Int = 0;
    var buildingsPlaced: Map[String, Int] = BuildingManager.typeByName.keySet.map(building => building -> 0).toMap
  }
  
  object Deals extends Serializable
  {
    var boughtDeals: Set[String]  = Set.empty
    var tokenSpent: Long  = 0L
    var usdtSpent: Long   = 0L
    var tonSpent: Long    = 0L
    var boosters: Array[BoosterJSON] = Array.empty
  }

  object ActiveBoosters extends Serializable
  {
    var OrderMoney: Option[OrderMoney]      = None
    var OrderItems: Option[OrderItems]      = None
    var WorkSpeed: Option[WorkSpeed]        = None
    var GrowSpeed: Option[GrowSpeed]    		= None
  }

  object Wallet extends Serializable
  {
    var tokenBalance: Long  = 0L
    var usdtBalance: Long   = 0L
    var tonBalance: Long    = 0L
  }
}