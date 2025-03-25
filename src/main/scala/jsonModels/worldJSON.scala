package jsonModels

import _root_.gameClasses._
import _root_.`<empty>`.ENVars
import zio.json.JsonEncoder
import zio.json.DeriveJsonEncoder
import zio.json.ast.Json
import zio.json._
import java.util.LinkedList
import java.time.{Instant, Duration}

val MapSettings = ENVars.GAME_SETTINGS.MAP

case class BuildingSlotJSON(workName: Option[String], workStartTimeStamp: Long, workEndTimeStamp: Long)
object BuildingSlotJSON {
  implicit val encoder: JsonEncoder[BuildingSlotJSON] = DeriveJsonEncoder.gen[BuildingSlotJSON]
}

case class BuildingJSON(name: String, x: Int, y: Int, slots: Array[BuildingSlotJSON], integerData: Option[Int], level: Option[Int])
object BuildingJSON {
  def fromTile(tile: Tile, x: Int, y: Int): Option[BuildingJSON] =
  {
    if (tile.place == null) None
    else {
        val building = tile.place;

        building match
          case _building: Garden => Some( 		fromGarden(_building, x, y))
          case _building: Bakery => Some( 		fromBakery(_building, x, y))
          case _building: Bush => Some( 		  fromBush(_building, x, y))
          case _building: Corral => Some( 		fromCorral(_building, x, y))
          case _building: Obstacle => Some(   fromObstacle(_building, x, y))
          case _ => None
      }
  }
  def fromGarden(garden: Garden, x: Int, y: Int): BuildingJSON = {
      val plant = if (garden.curplant != null) garden.curplant else "none"
      BuildingJSON("garden", x, y, Array(BuildingSlotJSON(Some(plant), garden.plantTimeStamp.getEpochSecond(), garden.growTimeStamp.getEpochSecond())), None, None)
    }
  def fromBakery(bakery: Bakery, x: Int, y: Int): BuildingJSON = {
      var buildingSlotsArray: Array[BuildingSlotJSON] = Array.empty;
      bakery.slots.iterator().forEachRemaining(slot => buildingSlotsArray = buildingSlotsArray :+ BuildingSlotJSON(Some(slot.name), slot.workStartTimeStamp.getEpochSecond(), slot.workEndTimeStamp.getEpochSecond()))
      BuildingJSON(bakery.getName(), x, y, buildingSlotsArray, Some(bakery.purchasedSlots), Some(bakery.curlevel));
    }
  def fromCorral(corral: Corral, x: Int, y: Int): BuildingJSON = {
      var buildingSlotsArray: Array[BuildingSlotJSON] = Array.empty;
      if (corral.workStartTimeStamp != null) buildingSlotsArray = buildingSlotsArray :+ BuildingSlotJSON(None, corral.workStartTimeStamp.getEpochSecond(), corral.workEndTimeStamp.getEpochSecond())
      BuildingJSON(corral.getName(), x, y, buildingSlotsArray, Some(corral.animalAmount), Some(corral.curlevel));
    }
  def fromBush(bush: Bush, x: Int, y: Int): BuildingJSON = {
      var buildingSlotsArray: Array[BuildingSlotJSON] = Array.empty;
      buildingSlotsArray = buildingSlotsArray :+ BuildingSlotJSON(None, bush.lastTimeCollected.getEpochSecond(), bush.readyTimeStamp.getEpochSecond())
      BuildingJSON(bush.getName(), x, y, buildingSlotsArray, Some(bush.collectedAmount), None)
    }
  def fromObstacle(obstacle: Obstacle, x: Int, y: Int): BuildingJSON = {
    BuildingJSON(obstacle.getName(), x, y, Array.empty, None, None)
  }
  implicit val encoder: JsonEncoder[BuildingJSON] = DeriveJsonEncoder.gen[BuildingJSON]
}

case class TileJSON(occupied: Boolean, place: String)
object TileJSON {
  implicit val encoder: JsonEncoder[TileJSON] = DeriveJsonEncoder.gen[TileJSON]
}

case class WorldJSON(tileArray: Array[BuildingJSON])
object WorldJSON {
  def fromWorld(world: World): WorldJSON = {
    var buildingArray: Array[BuildingJSON] = Array.empty
    var i = 0; var j = 0;
    while (i < MapSettings.SIZE.x) {
        j = 0;
        while (j < MapSettings.SIZE.y) {
            val buildingTemp = BuildingJSON.fromTile(world.tileArray(i)(j), i, j)
            val buildingToAdd: BuildingJSON = buildingTemp.getOrElse(null);
            if (buildingToAdd != null) buildingArray = buildingArray :+ buildingToAdd
            j+=1
          }
        i+=1
      }
    WorldJSON(buildingArray)
  }
  implicit val encoder: JsonEncoder[WorldJSON] = DeriveJsonEncoder.gen[WorldJSON]
}