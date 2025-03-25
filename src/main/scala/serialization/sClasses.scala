package serialization

import _root_.`<empty>`.Game
import _root_.gameClasses._

import java.time.Instant
import java.math.BigInteger
import java.util.LinkedList


case class sPlayer(
  Inventory: sItemsContainer,
  money: Int,
  netWorth: Int,
  spin: sSpin,
  orders: Array[sOrder],
  Businesses: Array[Option[sBusiness]],
  Stats: sStats,
  Deals: sDeals,
  ActiveBoosters: sActiveBoosters,
  Wallet: sWallet
)

case class sGame(
  player: sPlayer,
  world: sWorld,
  TRANSFERS_INFO: sTransferInfo,
  CLIENT_INFO: sClientInfo,
  lastOperation: Int
)

case class sTransferInfo(
  deposits: Array[sDeposit],
  withdraws: Array[sWithdraw]
)

case class sClientInfo(
  tgID: Long,
  refID: Long,
  wallet: String,
  banned: Boolean,
  strikes: Int, 
  referrals: Array[Long]
)

case class sWorld(tileArray: Array[Array[sTile]])

case class sTile(
  occupied: Boolean,
  place: sBuildable
)

case class sSpin(
  items: Array[(String, Int)],
  drop: (String, Int),
  activated: Boolean,
  generateTimeStamp: Instant
)

case class sOrder(
  items: Map[String, Int],
  price: Int,
  tokenPrice: Int,
  completed: Boolean,
  startTimeStamp: Instant
)

case class sItemsContainer(
  items: Map[String, Int],
  itemsAmount: Int,
  capacity: Int,
  level: Int
)

case class sDeposit(
  transaction_id: BigInteger,
  active: Boolean,
  tg_id: BigInteger,
  amount: BigInteger,
  time_stamp: BigInteger,
  jetton_signature: String,
  commentary: String
)

case class sWithdraw(
  transaction_id: BigInteger,
  status: Short,
  tg_id: BigInteger,
  wallet: String,
  amount: BigInteger,
  time_stamp: BigInteger
)

case class sBusiness(
  id: Int,
  lvl: Int,
  activateINFO: Option[(Instant, Int)]
)

case class sBoosterJSON(
  boosterType: String,
  percentage: Int,
  time: Int
)

sealed trait sBoosters {
  def percentage: Int
  def time: Int
  def activateTimeStamp: Instant
}

object sBoosters {
  case class OrderMoney(percentage: Int, time: Int, activateTimeStamp: Instant) extends sBoosters
  case class OrderItems(percentage: Int, time: Int, activateTimeStamp: Instant) extends sBoosters
  case class WorkSpeed(percentage: Int, time: Int, activateTimeStamp: Instant) extends sBoosters
  case class GrowSpeed(percentage: Int, time: Int, activateTimeStamp: Instant) extends sBoosters
}

case class sSlot(
  name: String,
  workStartTimeStamp: Instant,
  workEndTimeStamp: Instant
)

case class sBakery(
  bakeryType: String,
  curlevel: Int,
  slots: LinkedList[sSlot],
  lastEmptySlot: Int,
  purchasedSlots: Int,
  lastEndTime: Instant
) extends sBuildable

case class sBush(
  bushType: String,
  lastTimeCollected: Instant,
  readyTimeStamp: Instant,
  collectedAmount: Int
) extends sBuildable

case class sCorral(
  corralType: String,
  curlevel: Int,
  workStartTimeStamp: Instant,
  workEndTimeStamp: Instant,
  animalAmount: Int
) extends sBuildable

case class sGarden(
  curplant: String,
  plantTimeStamp: Instant,
  growTimeStamp: Instant
) extends sBuildable

case class sObstacle(
  name: String
) extends sBuildable

abstract class sBuildable

case class sReward(
  money: Option[Int],
  token: Option[Long],
  boosters: Option[Array[sBoosterJSON]]
)

case class sActiveBoosters(
  OrderMoney: Option[sBoosters.OrderMoney],
  OrderItems: Option[sBoosters.OrderItems],
  WorkSpeed: Option[sBoosters.WorkSpeed],
  GrowSpeed: Option[sBoosters.GrowSpeed]
)

case class sStats(
  ordersCompleted: Int,
  buildingsPlaced: Map[String, Int]
)

case class sDeals(
  boughtDeals: Set[String],
  boosters: Array[sBoosterJSON],
  tonSpent: Long,
  usdtSpent: Long,
  tokenSpent: Long
)

case class sWallet(
  tokenBalance: Long,
  usdtBalance: Long,
  tonBalance: Long
)

def convertGame(game: Game): sGame = {
  // Convert Player to sPlayer
  def convertPlayer(player: Player): sPlayer = {
    if (player.orders!=null)
    {
      sPlayer(
        Inventory = sItemsContainer(player.Inventory.items, player.Inventory.itemsAmount, player.Inventory.capacity, player.Inventory.level),
        money = player.money,
        netWorth = player.netWorth,
        spin = convertSpin(player.spin),
        orders = player.orders.map(convertOrder),
        Businesses = player.Businesses.businesses.map(convertBusiness).toArray,
        Stats = convertStats(player),
        Deals = convertDeals(player),
        ActiveBoosters = convertActiveBoosters(player),
        Wallet = convertWallet(player)
      )
    }
    else
    {
      sPlayer(
        Inventory = sItemsContainer(player.Inventory.items, player.Inventory.itemsAmount, player.Inventory.capacity, player.Inventory.level),
        money = player.money,
        netWorth = player.netWorth,
        spin = convertSpin(player.spin),
        orders = null,
        Businesses = player.Businesses.businesses.map(convertBusiness).toArray,
        Stats = convertStats(player),
        Deals = convertDeals(player),
        ActiveBoosters = convertActiveBoosters(player),
        Wallet = convertWallet(player)
      )
    }
  }

  // Convert World to sWorld
  def convertWorld(world: World): sWorld = {
    sWorld(tileArray = world.tileArray.map(_.map(convertTile)))
  }

  // Convert TRANSFERS_INFO to sTransferInfo
  def convertTransfersInfo(game: Game): sTransferInfo = {
    val info = game.TRANSFERS_INFO
    sTransferInfo(
      deposits = info.deposits.map(convertDeposit),
      withdraws = info.withdraws.map(convertWithdraw)
    )
  }

  // Convert CLIENT_INFO to sClientInfo
  def convertClientInfo(game: Game): sClientInfo = {
    val info = game.CLIENT_INFO
    sClientInfo(
      tgID = info.tgID,
      refID = info.refID,
      wallet = info.wallet,
      banned = info.banned,
      strikes = info.strikes,
      referrals = info.referrals
    )
  }

  // Convert Spin to sSpin
  def convertSpin(spin: Spin): sSpin = {
    if (spin != null)
    {
      sSpin(
        items = spin.items,
        drop = spin.drop,
        activated = spin.activated,
        generateTimeStamp = spin.generateTimeStamp
      )
    }
    else
    {
      null
    }
  }

  // Convert Order to sOrder
  def convertOrder(order: Order): sOrder = {
    sOrder(
      items = order.items,
      price = order.price,
      tokenPrice = order.tokenPrice,
      completed = order.completed,
      startTimeStamp = order.startTimeStamp
    )
  }

  // Convert Business to sBusiness
  def convertBusiness(business: Option[Business]): Option[sBusiness] = {
    business match
      case None => None
      case Some(b) => Some(sBusiness(id = b.ID, lvl = b.lvl, activateINFO = b.activateINFO))
  }

  // Convert Stats to sStats
  def convertStats(player: Player): sStats = {
    val stats = player.Stats
    sStats(
      ordersCompleted = stats.ordersCompleted,
      buildingsPlaced = stats.buildingsPlaced
    )
  }

  // Convert Deals to sDeals
  def convertDeals(player: Player): sDeals = {
    val deals = player.Deals
    sDeals(
      boughtDeals = deals.boughtDeals,
      boosters = deals.boosters.map(convertBoosterJSON),
      tonSpent = deals.tonSpent,
      usdtSpent = deals.usdtSpent,
      tokenSpent = deals.tokenSpent
    )
  }

  // Convert ActiveBoosters to sActiveBoosters
  def convertActiveBoosters(player: Player): sActiveBoosters = {
    val boosters = player.ActiveBoosters
    sActiveBoosters(
      OrderMoney = boosters.OrderMoney.map(convertOrderMoney),
      OrderItems = boosters.OrderItems.map(convertOrderItems),
      WorkSpeed = boosters.WorkSpeed.map(convertWorkSpeed),
      GrowSpeed = boosters.GrowSpeed.map(convertGrowSpeed)
    )
  }

  // Convert Wallet to sWallet
  def convertWallet(player: Player): sWallet = {
    val wallet = player.Wallet
    sWallet(
      tokenBalance = wallet.tokenBalance,
      usdtBalance = wallet.usdtBalance,
      tonBalance = wallet.tonBalance
    )
  }

  // Convert Tile to sTile
  def convertTile(tile: Tile): sTile = {
    sTile(
      occupied = tile.occupied,
      place = tile.place match {
        case b: Buildable => convertBuildable(b)
        case null => null
      }
    )
  }

  // Convert Buildable to sBuildable
  def convertBuildable(buildable: Buildable): sBuildable = buildable match {
    case b: Bakery => sBakery(b.bakeryType, b.curlevel, convertSlots(b.slots), b.lastEmptySlot, b.purchasedSlots, b.lastEndTime)
    case b: Bush => sBush(b.bushType, b.lastTimeCollected, b.readyTimeStamp, b.collectedAmount)
    case c: Corral => sCorral(c.corralType, c.curlevel, c.workStartTimeStamp, c.workEndTimeStamp, c.animalAmount)
    case g: Garden => sGarden(g.curplant, g.plantTimeStamp, g.growTimeStamp)
    case o: Obstacle => sObstacle(o.name)
  }

  def convertSlots(slots: LinkedList[Slot]): LinkedList[sSlot] = {
    val newList = new LinkedList[sSlot]();
    slots.forEach((slot) => {
      newList.addLast(convertSlot(slot))
    })
    newList
  }

  // Convert Slot to sSlot
  def convertSlot(slot: Slot): sSlot = {
    sSlot(
      name = slot.name,
      workStartTimeStamp = slot.workStartTimeStamp,
      workEndTimeStamp = slot.workEndTimeStamp
    )
  }

  // Convert BoosterJSON to sBoosterJSON
  def convertBoosterJSON(booster: BoosterJSON): sBoosterJSON = {
    sBoosterJSON(booster.boosterType, booster.percentage, booster.time)
  }

  // Convert individual Booster types
  def convertOrderMoney(b: Boosters.OrderMoney): sBoosters.OrderMoney = sBoosters.OrderMoney(b.percentage, b.time, b.activateTimeStamp)
  def convertOrderItems(b: Boosters.OrderItems): sBoosters.OrderItems = sBoosters.OrderItems(b.percentage, b.time, b.activateTimeStamp)
  def convertWorkSpeed(b: Boosters.WorkSpeed): sBoosters.WorkSpeed = sBoosters.WorkSpeed(b.percentage, b.time, b.activateTimeStamp)
  def convertGrowSpeed(b: Boosters.GrowSpeed): sBoosters.GrowSpeed = sBoosters.GrowSpeed(b.percentage, b.time, b.activateTimeStamp)

  // Convert Deposit to sDeposit
  def convertDeposit(deposit: Deposit): sDeposit = {
    sDeposit(
      transaction_id = deposit.transaction_id,
      active = deposit.active,
      tg_id = deposit.tg_id,
      amount = deposit.amount,
      time_stamp = deposit.time_stamp,
      jetton_signature = deposit.jetton_signature,
      commentary = deposit.commentary
    )
  }

  // Convert Withdraw to sWithdraw
  def convertWithdraw(withdraw: Withdraw): sWithdraw = {
    sWithdraw(
      transaction_id = withdraw.transaction_id,
      status = withdraw.status,
      tg_id = withdraw.tg_id,
      wallet = withdraw.wallet,
      amount = withdraw.amount,
      time_stamp = withdraw.time_stamp
    )
  }

  // Create and return the sGame instance
  sGame(
    player = convertPlayer(game.player),
    world = convertWorld(game.world),
    TRANSFERS_INFO = convertTransfersInfo(game),
    CLIENT_INFO = convertClientInfo(game),
    lastOperation = game.lastOperation
  )
}

def convertToSlot(sSlot: sSlot): Slot = {
  new Slot(
    workType = null,
    workTypeName = sSlot.name,
    timeStamp = sSlot.workStartTimeStamp,
    boostedTime = calculateBoostedTime(sSlot),
    sSlot.workStartTimeStamp
  )
}

def calculateBoostedTime(sSlot: sSlot): Int = {
  0
}

def convertsSlots(slots: LinkedList[sSlot]): LinkedList[Slot] = {
  val newList = new LinkedList[Slot]();
  slots.forEach((slot) => {
    newList.addLast(convertsSlot(slot))
  })
  newList
}

def convertsSlot(slot: sSlot): Slot = {
  Slot(
    null,
    slot.name,
    slot.workStartTimeStamp,
    0,
    slot.workEndTimeStamp
  )
}

def convertToGame(sgame: sGame): Game = {
  val game = new Game(sgame.CLIENT_INFO.tgID, sgame.CLIENT_INFO.refID)

  // Set player fields
  game.player.money = sgame.player.money
  game.player.netWorth = sgame.player.netWorth
  game.player.orders = sgame.player.orders.map(o => new Order(o.items, o.price, o.startTimeStamp, o.tokenPrice))
  
  game.player.Inventory.capacity = sgame.player.Inventory.capacity
  game.player.Inventory.items = sgame.player.Inventory.items
  game.player.Inventory.itemsAmount = sgame.player.Inventory.itemsAmount
  game.player.Inventory.level = sgame.player.Inventory.level

  // Convert spin
  val sSpin = sgame.player.spin
  game.player.spin = new Spin(sSpin.items, sSpin.drop)
  game.player.spin.activated = sSpin.activated
  game.player.spin.generateTimeStamp = sSpin.generateTimeStamp
  
  // Convert businesses
  game.player.Businesses.businesses = sgame.player.Businesses.map {
    case Some(sBusiness) => {
      val b = Business(sBusiness.id, sBusiness.lvl)
      b.activateINFO = sBusiness.activateINFO
      Some(b)
    }
    case None => None
  }

  // Convert stats
  game.player.Stats.ordersCompleted = sgame.player.Stats.ordersCompleted
  game.player.Stats.buildingsPlaced = sgame.player.Stats.buildingsPlaced

  // Convert deals
  game.player.Deals.boughtDeals = sgame.player.Deals.boughtDeals
  game.player.Deals.boosters = sgame.player.Deals.boosters.map(b => BoosterJSON(b.boosterType, b.percentage, b.time))
  game.player.Deals.tokenSpent = sgame.player.Deals.tokenSpent
  game.player.Deals.usdtSpent = sgame.player.Deals.usdtSpent
  game.player.Deals.tonSpent = sgame.player.Deals.tonSpent

  // Convert active boosters
  game.player.ActiveBoosters.OrderMoney = sgame.player.ActiveBoosters.OrderMoney.map(b => Boosters.OrderMoney(b.percentage, b.time, b.activateTimeStamp))
  game.player.ActiveBoosters.OrderItems = sgame.player.ActiveBoosters.OrderItems.map(b => Boosters.OrderItems(b.percentage, b.time, b.activateTimeStamp))
  game.player.ActiveBoosters.WorkSpeed = sgame.player.ActiveBoosters.WorkSpeed.map(b => Boosters.WorkSpeed(b.percentage, b.time, b.activateTimeStamp))
  game.player.ActiveBoosters.GrowSpeed = sgame.player.ActiveBoosters.GrowSpeed.map(b => Boosters.GrowSpeed(b.percentage, b.time, b.activateTimeStamp))
  
  // Convert wallet
  game.player.Wallet.tokenBalance = sgame.player.Wallet.tokenBalance
  game.player.Wallet.usdtBalance = sgame.player.Wallet.usdtBalance
  game.player.Wallet.tonBalance = sgame.player.Wallet.tonBalance

  // Convert world
  game.world.tileArray.indices.foreach { i =>
    game.world.tileArray(i).indices.foreach { j =>
      val sTile = sgame.world.tileArray(i)(j)
      val tile = game.world.tileArray(i)(j)
      tile.occupied = sTile.occupied
      tile.place = sTile.place match {
        case sBakery: sBakery =>
          val bakery = new Bakery(sBakery.bakeryType)
          bakery.curlevel = sBakery.curlevel
          bakery.slots = convertsSlots(sBakery.slots)
          bakery.lastEmptySlot = sBakery.lastEmptySlot
          bakery.purchasedSlots = sBakery.purchasedSlots
          bakery.lastEndTime = sBakery.lastEndTime
          bakery
        case sBush: sBush =>
          val bush = new Bush(sBush.bushType)
          bush.lastTimeCollected = sBush.lastTimeCollected
          bush.readyTimeStamp = sBush.readyTimeStamp
          bush.collectedAmount = sBush.collectedAmount
          bush
        case sCorral: sCorral =>
          val corral = new Corral(sCorral.corralType)
          corral.curlevel = sCorral.curlevel
          corral.workStartTimeStamp = sCorral.workStartTimeStamp
          corral.workEndTimeStamp = sCorral.workEndTimeStamp
          corral.animalAmount = sCorral.animalAmount
          corral
        case sGarden: sGarden =>
          val garden = new Garden()
          garden.curplant = sGarden.curplant
          garden.plantTimeStamp = sGarden.plantTimeStamp
          garden.growTimeStamp = sGarden.growTimeStamp
          garden
        case sObstacle: sObstacle =>
          val obstacle = new Obstacle(sObstacle.name)
          obstacle
        case null => null
      }
    }
  }

  // Set transfers info
  game.TRANSFERS_INFO.deposits = sgame.TRANSFERS_INFO.deposits.map(d =>
    Deposit(d.transaction_id, d.active, d.tg_id, d.amount, d.time_stamp, d.jetton_signature, d.commentary))
  game.TRANSFERS_INFO.withdraws = sgame.TRANSFERS_INFO.withdraws.map(w =>
    Withdraw(w.transaction_id, w.status, w.tg_id, w.wallet, w.amount, w.time_stamp))

  // Set client info
  game.CLIENT_INFO.wallet = sgame.CLIENT_INFO.wallet
  game.CLIENT_INFO.banned = sgame.CLIENT_INFO.banned
  game.CLIENT_INFO.strikes = sgame.CLIENT_INFO.strikes
  game.CLIENT_INFO.referrals = sgame.CLIENT_INFO.referrals

  // Set last operation
  game.lastOperation = sgame.lastOperation

  game
}
