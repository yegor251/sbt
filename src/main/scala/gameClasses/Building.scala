package gameClasses

import _root_.`<empty>`.ENVars
import _root_.`<empty>`.GE
import scala.quoted.Type

enum BuildingType:
	case garden, bakery, corral, bush

abstract class Buildable() extends Serializable {
	def collect(): Int
	def use(data: String): Int
	def upgrade(): Int
	def getName(): String
	def reQueue(player: Player): Unit
}

object BuildingManager{
	var typeByName: Map[String, BuildingType] = Map.empty
	ENVars.ASSETS.blist.forEach((building) => typeByName = typeByName.updated(building, BuildingType.bakery))
	ENVars.ASSETS.clist.forEach((building) => typeByName = typeByName.updated(building, BuildingType.corral))
	ENVars.ASSETS.bushlist.forEach((building) => typeByName = typeByName.updated(building, BuildingType.bush))
	typeByName = typeByName.updated("garden", BuildingType.garden)

	def buildingExists(name: String): Boolean = 			typeByName.isDefinedAt(name)
	def getBuildingType(building: String): BuildingType = 	typeByName(building);

	def getBuildingByName(building: String): Buildable = {
		val buildingType = getBuildingType(building)
		buildingType match
			case BuildingType.bakery => 	Bakery(building)
			case BuildingType.garden => 	Garden()
			case BuildingType.corral => 	Corral(building)
			case BuildingType.bush => 		Bush(building)
	}

	def getItemsByNameAndLevel(building: String, lvl: Int): Set[String] = {
		val buildingType = getBuildingType(building)
		buildingType match
			case BuildingType.bakery => BakeryManager.getItemsByNameAndLevel(building, lvl)
			case BuildingType.garden => Set.empty
			case BuildingType.corral => CorralManager.getCorral(building).products.keySet
			case BuildingType.bush => BushManager.getBush(building).products.keySet
	}

	def getPrice(building: String): Int = {
		val buildingType = getBuildingType(building)
		buildingType match
			case BuildingType.garden => 	GardenManager.getPrice();
			case BuildingType.bakery =>		BakeryManager.getPrice(building);
			case BuildingType.corral => 	CorralManager.getPrice(building);
			case BuildingType.bush =>		BushManager.getPrice(building);
	}

	def getSize(building: String): (Int, Int) =
	{
		val buildingType = getBuildingType(building)
		buildingType match
			case BuildingType.garden => 	(1, 1);
			case BuildingType.bakery =>		BakeryManager.getSize(building);
			case BuildingType.corral =>		CorralManager.getSize(building)
			case BuildingType.bush => 		BushManager.getSize(building)
	}

	def getMapLimit(building: String): Int =
	{
		val buildingType = getBuildingType(building)
		buildingType match
			case BuildingType.garden => 	GardenManager.getMapLimit()
			case BuildingType.bakery =>		BakeryManager.getMapLimit(building)
			case BuildingType.corral =>		CorralManager.getMapLimit(building)
			case BuildingType.bush => 		BushManager.getMapLimit(building)
	}

	def verifyPurchase(player: Player, building: String): Int ={
		val price = getBuildingType(building) match
			case BuildingType.bakery 	=> (getPrice(building) * Math.pow(ENVars.GAME_SETTINGS.BUILDINGS.building_bakery_per_building_k, player.Stats.buildingsPlaced(building))).toInt
			case BuildingType.garden 	=> (getPrice(building) * Math.pow(ENVars.GAME_SETTINGS.BUILDINGS.building_garden_per_building_k, player.Stats.buildingsPlaced(building))).toInt
			case BuildingType.corral 	=> (getPrice(building) * Math.pow(ENVars.GAME_SETTINGS.BUILDINGS.building_corral_per_building_k, player.Stats.buildingsPlaced(building))).toInt
			case BuildingType.bush 		=> getPrice(building)
		
		if (player.money >= price) {
			getItemsByNameAndLevel(building, 1).foreach((name) => {
				player.Inventory.addAmount(name, 0)
			})
			player.money -= price;
			GE.OK
		} else GE.NotEnoughMoney
	}

	def use(player: Player, building: Buildable, data: String): Int = {
		building match
			case _building: Garden => 		use(player, _building, data)
			case _building: Bakery => 		use(player, _building, data)
			case _building: Bush => 		use(player, _building, data)
			case _building: Corral => 		use(player, _building, data)
			case _ => GE.SocketWrongFormat
	}

	def collect(player: Player, building: Buildable): Int = {
		building match
			case _building: Garden => 		collect(player, _building)
			case _building: Bakery => 		collect(player, _building)
			case _building: Bush => 		collect(player, _building)
			case _building: Corral => 		collect(player, _building)
			case _ => GE.SocketWrongFormat
	}

	def upgrade(player: Player, building: Buildable): Int = {
		building match
			case _building: Bakery => 		upgrade(player, _building)
			case _building: Corral => 		upgrade(player, _building)
			case _ => GE.SocketWrongFormat
	}

	def purchaseSlot(player: Player, building: Buildable): Int = {
		building match
			case _building: Bakery => 		purchaseSlot(player, _building)
			case _ => GE.SocketWrongFormat
	}

			//	[goto management for garden]		
	def use(player: Player, building: Garden, data: String): Int = 	GardenManager.use(building, data, player);
	def collect(player: Player, building: Garden): Int = 			GardenManager.collect(building, player);

			//	[goto management for bakery]		
	def use(player: Player, building: Bakery, data: String): Int = 	BakeryManager.use(building, data, player);
	def collect(player: Player, building: Bakery): Int = 			BakeryManager.collect(building, player);
	def upgrade(player: Player, building: Bakery): Int = 			BakeryManager.upgrade(building, player);
	def purchaseSlot(player: Player, building: Bakery): Int =		BakeryManager.purchaseSlot(building, player)

			//	[goto management for corral]		
	def use(player: Player, building: Corral, data: String): Int = 	CorralManager.use(building, player, data);
	def collect(player: Player, building: Corral): Int = 			CorralManager.collect(building, player);
	def upgrade(player: Player, building: Corral): Int = 			CorralManager.upgrade(building, player);

			//	[goto management for bush]		
	def use(player: Player, building: Bush, data: String): Int = 	BushManager.use(building, player, data);
	def collect(player: Player, building: Bush): Int = 				BushManager.collect(building, player);
}