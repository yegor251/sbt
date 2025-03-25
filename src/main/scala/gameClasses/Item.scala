package gameClasses

import zio._
import zio.json._
import java.nio.file.{Files, Path}
import _root_.`<empty>`.ENVars
import ENVars.GAME_SETTINGS.ORDERS.ordersAmountMax

class Item() {}

case class ItemINFO(name: String, orderPrice: Int, orderTokenPrice: Int, orderAmountMin: Int, orderAmountMax: Int)
object ItemINFO{
  implicit val decoder: JsonDecoder[ItemINFO] = DeriveJsonDecoder.gen[ItemINFO]
}

object ItemManager
{
	var items: Map[String, ItemINFO] = Map.empty;

	for (name <- ENVars.ASSETS.ITEMS.keySet)
	{
		try 
		{
			items = items.updated(name, nameToLoadItem(name))
		}
		catch
		{
			case e: Exception => println(e.getMessage())
		}
	}

	def isLegalItem(name: String): Boolean = {
		(items.contains(name) || (Plants.plants.contains(name)))
	}

	def nameToItem(name: String): ItemINFO =
	{
		items.get(name).getOrElse(null)
	}

	def loadItem(path: Path): ItemINFO =
	{
		val bytes = Files.readAllBytes(path)
		val item = (new String(bytes, "UTF-8")).fromJson[ItemINFO].left.map(new Exception(_))
		item.getOrElse(null)
	}

	def nameToLoadItem(name: String): ItemINFO =
		loadItem(ENVars.ASSETS.ITEMS(name))

	def nameToPrice(name: String): Int =
	{
		if (items.contains(name)) nameToItem(name).orderPrice else
			if (Plants.plants.contains(name)) Plants.nameToPrice(name) else
				0
	}

	def nameToTokenPrice(name: String): Int =
	{
		if (items.contains(name)) nameToItem(name).orderTokenPrice else
			if (Plants.plants.contains(name)) Plants.nameToTokenPrice(name) else
				0
	}
	 
	 /** @return (min, max)
	 */
	def nameToOrderAmountLimits(name: String): (Int, Int) =
	{
		if (items.contains(name)) (nameToItem(name).orderAmountMin, nameToItem(name).orderAmountMax) else
			if (Plants.plants.contains(name)) Plants.nameToOrderAmountLimits(name) else
				(0,0)
	}
}