package gameClasses

import zio._
import zio.json._
import java.nio.file.{Files, Path}
import java.time.{Instant, Duration}
import _root_.`<empty>`.ENVars
import _root_.`<empty>`.GE
import java.io.Serial
import java.util.Queue._
import gameClasses.BakeryManager.getBakery
import java.util.LinkedList
import ENVars.GAME_SETTINGS.TRANSACTIONS.maxPurchasedSlots
import ENVars.GAME_SETTINGS.TRANSACTIONS.purchasedSlotsPrice


case class WorkTypeINFO(minLevel: Int, timeToFinish: Int, items: Map[String, Int], products: Map[String, Int])
case class BakeryINFO(name: String, price: Int, maxSlots: Int, sizex: Int, sizey: Int, maxLevel: Int, mapLimit: Int, workTypes: Map[String, WorkTypeINFO], upgradesPrice: Array[Int])

object WorkTypeINFO
{
  implicit val decoder: JsonDecoder[WorkTypeINFO] = DeriveJsonDecoder.gen[WorkTypeINFO]
}

object BakeryINFO
{
  implicit val decoder: JsonDecoder[BakeryINFO] = DeriveJsonDecoder.gen[BakeryINFO]
}

class Slot(workType: WorkTypeINFO, workTypeName: String, timeStamp: Instant, boostedTime: Int, endTimeStamp: Instant = null) extends Serializable
{
	val name = workTypeName;
	val workStartTimeStamp = timeStamp;
	val workEndTimeStamp = if (workType == null) endTimeStamp else workStartTimeStamp.plusSeconds(workType.timeToFinish - boostedTime);
}

class Bakery(name: String) extends Buildable
{
	val bakeryType = name;
	var curlevel = 1;
	//var slots: Array[Slot] = Array.fill(BakeryManager.getMaxSlots(bakeryType))(new Slot(null, null))
	var slots: LinkedList[Slot] = new LinkedList[Slot]
	var lastEmptySlot: Int = 0;
	var purchasedSlots: Int = 0;
	var lastEndTime: Instant = Instant.now();
	def collect(): Int = {
		lastEmptySlot-=1
		GE.OK
	}
	def use(data: String): Int = GE.SocketWrongFormat
	def use(data: String, boostedTime: Int): Int = {
		val curtime = Instant.now();
		val timeStamp = if (lastEmptySlot > 0) {
			if (lastEndTime.isBefore(curtime)) curtime else lastEndTime
		} else curtime
		val slot = new Slot(BakeryManager.getBakery(this.getName()).workTypes(data), data, timeStamp, boostedTime)
		lastEndTime = slot.workEndTimeStamp
		slots.add(slot)
		lastEmptySlot+=1
		GE.OK
	}
	def upgrade(): Int = {
		curlevel+=1;
		GE.OK
	}
	def getName(): String = bakeryType
	def reQueue(player: Player): Unit = {
		val booster = player.ActiveBoosters.WorkSpeed
		val (boosterPercentage, boosterTime) = BoosterManager.getInfo(booster)
		var remainingBoosterTime = boosterTime

		val newSlots = new LinkedList[Slot]
		val workTypes = BakeryManager.getBakery(getName()).workTypes

		var currentEndTime = Instant.now()

		slots.forEach { slot =>
			val workTypeInfo = workTypes(slot.name)

			val originalTimeToFinish = Duration.between(slot.workStartTimeStamp, slot.workEndTimeStamp).getSeconds.toInt

			val boostedTime = if (originalTimeToFinish > remainingBoosterTime)
			Math.round(remainingBoosterTime * boosterPercentage / 100).toInt
			else
			Math.round(originalTimeToFinish * boosterPercentage / 100).toInt

			remainingBoosterTime = Math.max(remainingBoosterTime - originalTimeToFinish, 0)

			val newStartTime = if (newSlots.isEmpty) slot.workStartTimeStamp else currentEndTime
			val newSlot = new Slot(workTypeInfo, slot.name, newStartTime, boostedTime)

			currentEndTime = newSlot.workEndTimeStamp

			newSlots.add(newSlot)
		}

		slots = newSlots
		lastEndTime = currentEndTime
	}

}

object BakeryManager
{
	var bakeriesINFO: Map[String, BakeryINFO] = Map.empty
	var bakeriesItemByLevel: Map[String, Map[Int, Set[String]]] = Map.empty;

	for (name <- ENVars.ASSETS.BAKERIES.keySet) {
		val bInfo = nameToLoadBakery(name)
		bakeriesINFO = bakeriesINFO.updated(name, bInfo)
		bakeriesItemByLevel = bakeriesItemByLevel.updated(name, Map.empty)

		bInfo.workTypes.map((wname, worktype) => {
			val newSet: Set[String] = Set.empty;
			val newMap = bakeriesItemByLevel(name).updated(worktype.minLevel, newSet)
			bakeriesItemByLevel = bakeriesItemByLevel.updated(name, newMap)
		})
		
		bInfo.workTypes.map((wname, worktype) => {
			val newSet = bakeriesItemByLevel(name)(worktype.minLevel).incl(wname)
			val newMap = bakeriesItemByLevel(name).updated(worktype.minLevel, newSet)
			bakeriesItemByLevel = bakeriesItemByLevel.updated(name, newMap)
		})
	}

	def nameToLoadBakery(name: String): BakeryINFO = loadBakery(ENVars.ASSETS.BAKERIES(name))

	def loadBakery(path: Path): BakeryINFO = {
		val bytes = Files.readAllBytes(path)
		val bakery = (new String(bytes, "UTF-8")).fromJson[BakeryINFO].left.map(new Exception(_))
		bakery.getOrElse(null)
	}


	def getMapLimit(building: String): Int = bakeriesINFO(building).mapLimit
	def getItemsByNameAndLevel(building: String, lvl: Int): Set[String] = bakeriesItemByLevel(building)(lvl)
	def getPrice(building: String): Int = bakeriesINFO(building).price
	def getMaxSlots(building: String): Int = bakeriesINFO(building).maxSlots
	def getBakery(name: String): BakeryINFO = bakeriesINFO(name)
	def getSize(name: String): (Int, Int) = (bakeriesINFO(name).sizex, bakeriesINFO(name).sizey)

	def use(bakery: Bakery, data: String, player: Player): Int = {
		val bakeryinfo = getBakery(bakery.getName())
		val worktypeinfo = bakeryinfo.workTypes.get(data).getOrElse(null)

		if (worktypeinfo != null) {
			var validated = true;
			for ((item, amount) <- worktypeinfo.items) {
				if (player.Inventory.getAmount(item).getOrElse(0) < amount) validated = false
			}
			if (bakery.curlevel < worktypeinfo.minLevel) validated = false
			if (bakery.lastEmptySlot >= (bakeryinfo.maxSlots + bakery.purchasedSlots)) validated = false

			if (validated) {
				for ((item, amount) <- getBakery(bakery.getName()).workTypes(data).items){
					player.Inventory.addAmount(item, -amount)
				}
				val boosterP_T = BoosterManager.getInfo(player.ActiveBoosters.WorkSpeed)

				val timeBeforeBooster = Math.max(bakery.lastEndTime.getEpochSecond() - Instant.now().getEpochSecond(), 0)

				val boosterTime = Math.max((boosterP_T._2 - timeBeforeBooster), 0)

				val boostedTime = if (worktypeinfo.timeToFinish > boosterTime) 
					Math.round(boosterTime * boosterP_T._1 / 100).toInt
				else 
					Math.round(worktypeinfo.timeToFinish * boosterP_T._1 / 100).toInt
				
				bakery.use(data, boostedTime)
				GE.OK
			}
			else GE.NotEnoughBuildingConditions
		}
		else GE.SocketWrongFormat
	}

	def collect(bakery: Bakery, player: Player): Int = {
		val curtime = Instant.now();
		val slot = try bakery.slots.peek() catch _ => null

		if (slot == null) GE.WorkNotStarted
		else if (slot.name == null) GE.InternalServerLogicError
		else if (slot.workEndTimeStamp.isAfter(curtime)) GE.WorkNotReady
		else {
			val bakeryinfo = getBakery(bakery.getName())
			val worktypeinfo = bakeryinfo.workTypes(slot.name)
			if (player.Inventory.checkMap(worktypeinfo.products)) {
				player.Inventory.addMapUnchecked(worktypeinfo.products)
				bakery.collect()
				bakery.slots.removeFirst()
				GE.OK
			} else GE.NoSpaceInInventory
		}
	}

	def upgrade(bakery: Bakery, player: Player): Int = {
		val bakeryName = bakery.getName()
		val bakeryinfo = getBakery(bakeryName)
		if (bakery.curlevel < bakeryinfo.maxLevel) {
			val price = bakeryinfo.upgradesPrice(bakery.curlevel - 1)
			if (player.money >= price) {
				bakeriesItemByLevel(bakeryName).get(bakery.curlevel+1).getOrElse(Set.empty).foreach((name) =>{
					player.Inventory.addAmount(name, 0)
				})
				player.money -= price
				bakery.upgrade();
			} else GE.NotEnoughMoney
		} else GE.BuildingAlreadyMaxxedUp
	}

	def purchaseSlot(bakery: Bakery, player: Player): Int = {
		val purchasedSlots = bakery.purchasedSlots;
		val maxPurchasedSlots = ENVars.GAME_SETTINGS.TRANSACTIONS.maxPurchasedSlots;

		if (purchasedSlots < maxPurchasedSlots) {
			val purchasedSlotsPrice = ENVars.GAME_SETTINGS.TRANSACTIONS.purchasedSlotsPrice(purchasedSlots);
			if (player.Wallet.tokenBalance >= purchasedSlotsPrice) {
				player.Wallet.tokenBalance -= purchasedSlotsPrice;
				bakery.purchasedSlots += 1;
				GE.OK
			} else GE.NotEnoughMoney
		} else GE.BuildingAlreadyMaxxedUp
	}
}