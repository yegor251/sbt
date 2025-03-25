import gameClasses._
import _root_.serialization._
import zio.json._
import java.math.BigInteger

class Game(telegram_ID: Long, ref_id: Long) extends Serializable{
  val player: Player = Player();
  val world: World = World();

  object TRANSFERS_INFO extends Serializable{
    var deposits: Array[Deposit] = Array.empty;
    var withdraws: Array[Withdraw] = Array.empty;
  }

  object CLIENT_INFO extends Serializable{
    val tgID: Long = telegram_ID;
    val refID: Long = ref_id;
    var wallet: String = "no"
    var banned = false;
    var strikes = 0;
    var referrals: Array[Long] = Array.empty;
  }

  var lastOperation: Int = 0;

  def firstInit() = {
    player.Inventory.addAmount("wheat", 4);
    World.defaultMap.map.foreach((el) => {
      ObstacleManager.___PLACE_OBSTACLE_UNSAFE(this, el.x, el.y, el.name)
    })
  }

  def getState(): String = {
    lastOperation match
      case GE.Connected                         => s"${GameJSON.fromGame(this).toJson}"
      case GE.Regeneration                      => s"${GameJSON.fromGameRegen(this).toJson}"

      case _: Int                               => s"${OperationJSON.fromCode(this.lastOperation).toJson}"
  }
  def onBan() = {CLIENT_INFO.banned = true}
  def onUnban() = {CLIENT_INFO.banned = false}
  def onConnect() = {
    onRegenerate();
    DepositManager.check(this);
    WithdrawManager.checkAllWithdraws(this);
    lastOperation = GE.Connected;
  }
  def onUse(item: String, x: Int, y: Int) = {
    if ((world.validateUse(x, y) == GE.OK))
      lastOperation =                                       BuildingManager.use(player, world.getBuilding(x,y), item)
    else lastOperation = GE.BuildingNotPlaced
  }
  def onCollect(x: Int, y: Int) = {
    if ((world.validateUse(x, y) == GE.OK))
      lastOperation =                                       BuildingManager.collect(player, world.getBuilding(x,y))
    else lastOperation = GE.BuildingNotPlaced
  }
  def onBuy(item: String, amount: Int) = { lastOperation = Shop.buy(item, amount, player) }
  def onPlace(building: String, x: Int, y: Int) =
  {
    if (player.Stats.buildingsPlaced(building) < BuildingManager.getMapLimit(building))
    {
      val size = BuildingManager.getSize(building)
      if (world.validatePlace(x, y, size) == GE.OK)
      {
        if (BuildingManager.verifyPurchase(player, building) == GE.OK)
          lastOperation =                                     world.place(building, x, y, size);
        else lastOperation = GE.NotEnoughMoney
      } else lastOperation = GE.NotAbleToPlace
      if (lastOperation == GE.OK) {
        val newValue = player.Stats.buildingsPlaced(building) + 1
        player.Stats.buildingsPlaced = player.Stats.buildingsPlaced.updated(building, newValue)
      }
    } else lastOperation = GE.BuildingLimitExceeded
  }
  def onMove(x: Int, y: Int, to_x: Int, to_y: Int) = {
    lastOperation =                                         world.moveBuilding(x, y, to_x, to_y)}
  def onRegenerate() = {
    SpinManager.regenerate(player);
    OrderManager.regenerate(player);
    BoosterManager.regenerate(player);
    lastOperation = GE.Regeneration;
  }
  def onSpin() = { lastOperation =                          SpinManager.spinWheel(player) }
  def onCompleteOrder(orderID: Int) = { lastOperation =     OrderManager.completeOrder(player, orderID) }
  def onRerollOrder(orderID: Int) = { lastOperation =       OrderManager.rerollOrder(player, orderID) }
  def onUpgrade(x: Int, y: Int) = { if ((world.validateUse(x, y) == GE.OK))
      lastOperation =                                       BuildingManager.upgrade(player, world.getBuilding(x,y))
    else lastOperation = GE.BuildingNotPlaced
  }
  def onClaimDeposit(depId: Int) = { lastOperation =        DepositManager.claim(this, depId) }
  def onRegisterWithdraw(amount: Long, wallet: String) = { this.CLIENT_INFO.wallet = wallet; lastOperation = WithdrawManager.createWithdraw(this, amount, this.CLIENT_INFO.wallet) }
  def onPurchaseSlot(x: Int, y: Int) = { if ((world.validateUse(x, y) == GE.OK))
      lastOperation =                                       BuildingManager.purchaseSlot(player, world.getBuilding(x,y))
    else lastOperation = GE.BuildingNotPlaced
  }
  def onPurchaseDeal(dealName: String) = { 
    if (!player.Deals.boughtDeals(dealName))
      lastOperation =                                       Shop.purchaseDeal(player, dealName)
    else lastOperation = GE.DealHasBeenAlreadyBought
  }
  def onActivateBooster(boosterId: Int) = {
    lastOperation =                                         BoosterManager.activate(player, boosterId)
    world.reQueueAll(player)
  }
  def onInventoryUpgrade() = { lastOperation =              Ambar.upgradeInventory(player) }
  def onPurchaseBusiness(id: Int) = { lastOperation =       BusinessManager.purchaseBusiness(player, id) }
  def onUpgradeBusiness(id: Int) = { lastOperation =        BusinessManager.upgradeBusiness(player, id) }
  def onActivateBusiness(id: Int, amount: Int) = {
    lastOperation =                                         BusinessManager.activate(player, id, amount)
  }
  def onCollectBusiness(id: Int) = { lastOperation =        BusinessManager.collect(player, id) }
  def onRemoveObstacle(x: Int, y: Int) = { lastOperation =  ObstacleManager.removeObstacle(this, x, y)}
}

