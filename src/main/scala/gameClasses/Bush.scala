package gameClasses

import zio.json.JsonDecoder
import zio.json.DeriveJsonDecoder
import java.time.Instant
import _root_.`<empty>`.ENVars
import _root_.`<empty>`.GE
import java.nio.file.{Files, Path}
import java.io.Serial
import zio._
import zio.json._
import zio.prelude.Derive


case class BushINFO(name: String, price: Int, productLimit: Int,
                    speed: Int,
                    sizex: Int, sizey: Int,
					mapLimit: Int,
                    products: Map[String, Int])

object BushINFO
{
	implicit val decoder: JsonDecoder[BushINFO] = DeriveJsonDecoder.gen[BushINFO]
}

class Bush(name: String) extends Buildable
{
	val bushType = name;
	var lastTimeCollected = Instant.now();
	var readyTimeStamp = lastTimeCollected.plusSeconds(BushManager.getBush(bushType).speed);
	var collectedAmount = 0;

	def collect(): Int = GE.SocketWrongFormat
	def collect(boostedTime: Int): Int =
	{
		collectedAmount+=1;
		lastTimeCollected = Instant.now();
		readyTimeStamp = lastTimeCollected.plusSeconds(BushManager.getBush(bushType).speed - boostedTime)
		GE.OK
	}

	def use(data: String): Int =
	{
		collectedAmount = 0;
		GE.OK
	}

	def upgrade(): Int =
	{
		GE.SocketWrongFormat
	}
	
	def getName(): String = 
	{
		bushType
	}
	def reQueue(player: Player) = {
		val booster = player.ActiveBoosters.WorkSpeed;
		val _bPT = BoosterManager.getInfo(booster)
		var _bT = _bPT._2
		val _bP = _bPT._1
		val timeToFinish = Math.max((readyTimeStamp.getEpochSecond() - Instant.now().getEpochSecond()).toInt, 0)

		val boostedTime = if (timeToFinish > _bT) 
			Math.round(_bT * _bP / 100).toInt
		else 
			Math.round(timeToFinish * _bP / 100).toInt

		readyTimeStamp = readyTimeStamp.minusSeconds(boostedTime)
	}
}

object BushManager
{
	var bushesINFO: Map[String, BushINFO] = Map.empty;

	for (name <- ENVars.ASSETS.BUSHES.keySet)
	{
		bushesINFO = bushesINFO.updated(name, nameToLoadCorrals(name))
	}

	def nameToLoadCorrals(name: String): BushINFO =
		loadCorral(ENVars.ASSETS.BUSHES(name))

	def loadCorral(path: Path): BushINFO =
	{
		val bytes = Files.readAllBytes(path)
		val bush = (new String(bytes, "UTF-8")).fromJson[BushINFO].left.map(new Exception(_))
		bush.getOrElse(null)
	}


	def getMapLimit(building: String): Int = bushesINFO(building).mapLimit
	def getBush(name: String): BushINFO = bushesINFO.get(name).getOrElse(null)
	def getSize(name: String): (Int, Int) =
	{
		val bushInf = getBush(name);
		(bushInf.sizex, bushInf.sizey)
	}

	def getPrice(name: String): Int =
	{
		getBush(name).price
	}

	def use(bush: Bush, player: Player, data: String): Int =
	{
		val bushInf = getBush(bush.getName());
		val newPrice = Math.round(bushInf.price * 1.5).toInt

		if (bush.collectedAmount >= bushInf.productLimit) {
			if (player.money > newPrice) {
				player.money = player.money - newPrice;
				bush.use(null);
			}
			else GE.NotEnoughMoney
		}
		else GE.BushNotExpired
	}

	def collect(bush: Bush, player: Player): Int =
	{
		val curtime = Instant.now();
		val bushInf = getBush(bush.getName());

		if (bush.readyTimeStamp.isBefore(curtime))
		{
			if (bush.collectedAmount < bushInf.productLimit)
			{
				if (player.Inventory.checkMap(bushInf.products)) {
					player.Inventory.addMapUnchecked(bushInf.products)

					val boosterP_T = BoosterManager.getInfo(player.ActiveBoosters.GrowSpeed)
					val timeToFinish = bushInf.speed
					val boostedTime = if (timeToFinish > boosterP_T._2) 
						Math.round(boosterP_T._2 * boosterP_T._1 / 100).toInt
					else 
						Math.round(timeToFinish * boosterP_T._1 / 100).toInt

					bush.collect(boostedTime)
				} else GE.NoSpaceInInventory
			}
			else
			{
				GE.BushExpired
			}
		}
		else
		{
			GE.WorkNotReady
		}
	}
	
	def upgrade(bush: Bush, player: Player): Int =
	{
		bush.upgrade();
	}
}

