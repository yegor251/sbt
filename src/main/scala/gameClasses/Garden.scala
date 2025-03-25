package gameClasses

import java.time.{Instant, Duration}
import zio._
import zio.json._
import java.nio.file.{Files, Path}
import _root_.`<empty>`.ENVars
import _root_.`<empty>`.GE
import java.sql.Date
import zio.http.Header.IfRange.DateTime

case class GardenINFO(price: Int, name: String, mapLimit: Int)

object GardenINFO
{
	implicit val decoder: JsonDecoder[GardenINFO] = DeriveJsonDecoder.gen[GardenINFO]
}

class Garden() extends Buildable
{
	var curplant: String = null
	var plantTimeStamp = Instant.now();
	var growTimeStamp = Instant.now();

	def use(data: String): Int = GE.SocketWrongFormat
	def plant(newplant: String, boosterPercentage: Int): Int =
	{
		curplant = newplant
		plantTimeStamp = Instant.now()
		growTimeStamp = plantTimeStamp.plusSeconds(Math.round(Plants.getTimeToGrow(newplant) * (1 - boosterPercentage * 0.01)).toInt)
		GE.OK
	}
	def collect(): Int =
	{
		val curTime = Instant.now();
		curplant = null
		plantTimeStamp = curTime
		growTimeStamp = curTime
		GE.OK
	}

	def upgrade(): Int =
	{
		GE.SocketWrongFormat
	}

	def getName(): String =
	{
		"garden"
	}
	def reQueue(player: Player) = {
		val booster = player.ActiveBoosters.WorkSpeed;
		val _bPT = BoosterManager.getInfo(booster)
		var _bT = _bPT._2
		val _bP = _bPT._1
		val timeToFinish = Math.max((growTimeStamp.getEpochSecond() - Instant.now().getEpochSecond()).toInt, 0)

		val boostedTime = if (timeToFinish > _bT) 
			Math.round(_bT * _bP / 100).toInt
		else 
			Math.round(timeToFinish * _bP / 100).toInt

		growTimeStamp = growTimeStamp.minusSeconds(boostedTime)
	}
}

object GardenManager
{
	val path = ENVars.ASSETS.garden;
	val bytes = Files.readAllBytes(path)
	val gardenINFO = (new String(bytes, "UTF-8")).fromJson[GardenINFO].left.map(new Exception(_)).getOrElse(null)
	if (gardenINFO == null) println("[FATAL ERROR] Cannot read gardenINFO")


	def getMapLimit(): Int = gardenINFO.mapLimit
	def collect(garden: Garden, player: Player): Int =
	{
		val curTime = Instant.now();
		val curplant = garden.curplant;
		if (curplant != null)
		{
			if (garden.growTimeStamp.isBefore(curTime))
			{
				if (player.Inventory.checkAmount(Plants.getGrowAmount(curplant))) {
					garden.collect();
					player.Inventory.addAmount(curplant, Plants.getGrowAmount(curplant))
					GE.OK
				} else GE.NoSpaceInInventory
			}
			else
			{
				GE.PlantNotGrown
			}
		}
		else
		{
			GE.PlantNotPlanted
		}
	}

	def use(garden: Garden, data: String, player: Player): Int =
	{
		if (Plants.plantDefined(data)) {
			if (garden.curplant == null) {
				if (player.Inventory.getAmount(data).getOrElse(0) < 1) GE.NotEnoughBuildingConditions
				else {
					player.Inventory.addAmount(data, -1);
					val boosterPercentage = BoosterManager.getPercentage(player.ActiveBoosters.GrowSpeed)

					garden.plant(data, boosterPercentage);
					GE.OK
				}
			}
			else GE.CannotUseBuilding
		}
		else GE.SocketWrongFormat
	}

	def getPrice(): Int =
	{
		gardenINFO.price
	}

	def upgrade(garden: Garden, player: Player): Int =
	{
		garden.upgrade()
	}
}