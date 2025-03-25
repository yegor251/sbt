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

case class CorralINFO(name: String, price: Int, speed: Int, 
					maxAnimalAmount: Int,
					animalPrice: Int,
					sizex: Int, sizey: Int,
					maxLevel: Int,
					mapLimit: Int,
					intake: Map[String, Int],
					products: Map[String, Int],
					upgradesPrice: Array[Int])

object CorralINFO
{
	implicit val decoder: JsonDecoder[CorralINFO] = DeriveJsonDecoder.gen[CorralINFO]
}

class Corral(name: String) extends Buildable
{
	val corralType: String = name;
	var curlevel = 1;
	var workStartTimeStamp: Instant = null;
	var workEndTimeStamp: Instant = null;
	var animalAmount: Int = 0;

	def collect(): Int =
	{
		workStartTimeStamp = null;
		workEndTimeStamp = null;
		GE.OK
	}

	def use(data: String): Int = GE.SocketWrongFormat
	def use(data: String, boostedTime: Int): Int =
	{
		workStartTimeStamp = Instant.now();
		val workTime = CorralManager.getTimeToFinish(this, curlevel)- boostedTime
		workEndTimeStamp = workStartTimeStamp.plusSeconds(workTime)
		GE.OK
	}

	def buyAnimal(): Int =
	{
		animalAmount+=1;
		GE.OK
	}

	def upgrade(): Int =
	{
		curlevel+=1;
		GE.OK
	}

	def getName(): String =
	{
		corralType
	}
	def reQueue(player: Player): Unit = {
		if (this.workEndTimeStamp != null) {
			val booster = player.ActiveBoosters.WorkSpeed;
			val _bPT = BoosterManager.getInfo(booster)
			var _bT = _bPT._2
			val _bP = _bPT._1
			val timeToFinish = Math.max((workEndTimeStamp.getEpochSecond() - Instant.now().getEpochSecond()).toInt, 0)

			val boostedTime = if (timeToFinish > _bT) 
					Math.round(_bT * _bP / 100).toInt
				else 
					Math.round(timeToFinish * _bP / 100).toInt

			workEndTimeStamp = workEndTimeStamp.minusSeconds(boostedTime)
		}
	}
}

object CorralManager
{
	var corralsINFO: Map[String, CorralINFO] = Map.empty;

	for (name <- ENVars.ASSETS.CORRALS.keySet)
	{
		corralsINFO = corralsINFO.updated(name, nameToLoadCorrals(name))
	}

	def nameToLoadCorrals(name: String): CorralINFO =
		loadCorral(ENVars.ASSETS.CORRALS(name))

	def loadCorral(path: Path): CorralINFO =
	{
		val bytes = Files.readAllBytes(path)
		val corral = (new String(bytes, "UTF-8")).fromJson[CorralINFO].left.map(new Exception(_))
		corral.getOrElse(null)
	}


	def getMapLimit(building: String): Int = corralsINFO(building).mapLimit
	def getCorral(name: String): CorralINFO =
	{
		corralsINFO.get(name).getOrElse(null)
	}

	def getSize(name: String): (Int, Int) =
	{
		val corralInf = getCorral(name);
		(corralInf.sizex, corralInf.sizey)
	}

	def getPrice(name: String): Int =
	{
		 getCorral(name).price
	}

	def collect(corral: Corral, player: Player): Int =
	{
		val curtime = Instant.now();
		
		if (corral.workStartTimeStamp != null)
		{
			if (corral.workEndTimeStamp.isBefore(curtime))
			{
				val products = CorralManager.getCorral(corral.corralType).products.map((s, i) => (s -> i*corral.animalAmount))
				if (player.Inventory.checkMap(products)) {
					player.Inventory.addMapUnchecked(products)
					corral.collect();
				} else GE.NoSpaceInInventory
			}
			else
			{
				GE.WorkNotReady
			}
		}
		else
		{
			GE.WorkNotStarted
		}
	}

	def buyAnimal(corral: Corral, player: Player): Int =
	{
		val corralInf = getCorral(corral.corralType)
		val price = corralInf.animalPrice
		if (corral.animalAmount < corralInf.maxAnimalAmount)
		{
			if (player.money >= price)
			{
				player.money -= price;
				corral.buyAnimal()
			}
			else
			{
				GE.NotEnoughMoney
			}
		}
		else
		{
			GE.CorralAlreadyMaxAnimals
		}
	}

	def getTimeToFinish(corralInf: CorralINFO, level: Int): Int = Math.round(corralInf.speed * (1 - (0.1 * (level - 1)))).toInt
	def getTimeToFinish(corral: Corral, level: Int): Int = Math.round(getCorral(corral.getName()).speed * (1 - (0.1 * (level - 1)))).toInt

	def startWork(corral: Corral, player: Player): Int =
	{
		val corralInf = getCorral(corral.corralType)
		if (corral.workEndTimeStamp == null)
		{
			var ableToWork = true;
			corralInf.intake.map((intake) => {
				if (player.Inventory.getAmount(intake._1).getOrElse(0) < intake._2 * corral.animalAmount)
				{
					ableToWork = false;
				}
			})
			if (ableToWork)
			{
				corralInf.intake.map((intake) => {
					player.Inventory.addAmount(intake._1, -intake._2 * corral.animalAmount)
				})
				val boosterP_T = BoosterManager.getInfo(player.ActiveBoosters.WorkSpeed)
				val timeToFinish = getTimeToFinish(corralInf, corral.curlevel)
				val boostedTime = if (timeToFinish > boosterP_T._2) 
					Math.round(boosterP_T._2 * boosterP_T._1 / 100).toInt
				else 
					Math.round(timeToFinish * boosterP_T._1 / 100).toInt
				
				corral.use(null, boostedTime)
			}
			else
			{
				GE.NotEnoughBuildingConditions
			}
		}
		else
		{
			GE.WorkNotReady
		}
	}

	def use(corral: Corral, player: Player, data: String): Int =
	{
		data match
			case "buy" => buyAnimal(corral, player)
			case "start" => startWork(corral, player)
			case _ => GE.SocketWrongFormat
	}

	def upgrade(corral: Corral, player: Player): Int =
	{
		val corralinfo = getCorral(corral.getName())
		if (corral.curlevel < corralinfo.maxLevel)
		{
			if (player.money > corralinfo.upgradesPrice(corral.curlevel - 1))
			{
				corral.upgrade()
			}
			else
			{
				GE.NotEnoughMoney
			}
		}
		else
		{
			GE.BuildingAlreadyMaxxedUp
		}
	}
}