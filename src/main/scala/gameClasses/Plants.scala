package gameClasses

import zio._
import zio.json._
import java.nio.file.{Files, Path}
import _root_.`<empty>`.ENVars

case class Seed(minimumLvl: Int, price: Int, amount: Int, timeToGrow: Int)
case class Plant(name: String, orderPrice: Int, orderTokenPrice: Int, seed: Seed, orderAmountMin: Int, orderAmountMax: Int) extends Item


object Seed {
  implicit val decoder: JsonDecoder[Seed] = DeriveJsonDecoder.gen[Seed]
}

object Plant {
  implicit val decoder: JsonDecoder[Plant] = DeriveJsonDecoder.gen[Plant]
}

object Plants
{
  var plants: Map[String, Plant] = Map.empty;

  for (name <- ENVars.ASSETS.SEEDS.keySet)
  {
    plants = plants.updated(name, nameToLoadPlant(name))
  }

  def test(name: String) = 
  {
    println(s"PLANTS_CLASS_TEST:${name}")
  }

  def nameToPlant(name: String): Plant =
  {
    plants.get(name).getOrElse(null)
  }

  def getPrice(name: String): Int =
  {
    nameToPlant(name).seed.price;
  }

  def getTimeToGrow(name: String): Int =
  {
    nameToPlant(name).seed.timeToGrow;
  }

  def getGrowAmount(name: String): Int =
  {
    nameToPlant(name).seed.amount;
  }

  def plantDefined(name: String): Boolean =
  {
    plants.isDefinedAt(name)
  }

  def loadPlant(path: Path): Plant =
    {
      val bytes = Files.readAllBytes(path)
      val plant = (new String(bytes, "UTF-8")).fromJson[Plant].left.map(new Exception(_))
      plant.getOrElse(null)
    }

  def nameToLoadPlant(name: String): Plant =
    loadPlant(ENVars.ASSETS.SEEDS(name))

  def nameToOrderAmountLimits(name: String): (Int, Int) =
  {
    (nameToPlant(name).orderAmountMin, nameToPlant(name).orderAmountMax)
  }

  def nameToPrice(name: String): Int =
    {
      nameToPlant(name).orderPrice
    }
  
  def nameToTokenPrice(name: String): Int =
    {
      nameToPlant(name).orderTokenPrice
    }
}