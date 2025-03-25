package gameClasses

import zio.json._
import java.nio.file.{Files, Path}
import _root_.`<empty>`.GE
import _root_.`<empty>`.ENVars

val MapSettings = ENVars.GAME_SETTINGS.MAP

case class StartBuild(name: String, x: Int, y: Int)
object StartBuild
{
	implicit val decoder: JsonDecoder[StartBuild] = DeriveJsonDecoder.gen[StartBuild]
}
case class WorldINFO(map: Array[StartBuild])
object WorldINFO
{
	implicit val decoder: JsonDecoder[WorldINFO] = DeriveJsonDecoder.gen[WorldINFO]
}

object World {
  val bytes = Files.readAllBytes(MapSettings.map)
  val defaultMap = (new String(bytes, "UTF-8")).fromJson[WorldINFO].left.map(new Exception(_)).getOrElse(WorldINFO(Array.empty))

  def debug() = {
    defaultMap.map.foreach((el) => {println(ObstacleManager.isObstalce(el.name))})
  }
}

class World extends Serializable{
  val tileArray: Array[Array[Tile]] = Array.fill(MapSettings.SIZE.x, MapSettings.SIZE.y)(new Tile)
  def place(building: String, x: Int, y: Int, size: (Int, Int)): Int = {
    var tx = 0
    var ty = 0
    while (tx < size._1)
        {
          var ty = 0
          while (ty < size._2)
          {
            tileArray(tx + x)(ty + y).occupied = true
            ty+=1
          }
          tx+=1;
        }
    tileArray(x)(y).place(building)
  }
  def place(building: Buildable, x: Int, y: Int, size: (Int, Int)): Int = {
    var tx = 0
    var ty = 0
    while (tx < size._1)
        {
          var ty = 0
          while (ty < size._2)
          {
            tileArray(tx + x)(ty + y).occupied = true
            ty+=1
          }
          tx+=1;
        }
    tileArray(x)(y).place = building
    GE.OK
  }
  def removeBuilding(x: Int, y: Int, size: (Int, Int)) = {
    var tx = 0
    var ty = 0
    while (tx < size._1)
        {
          var ty = 0
          while (ty < size._2)
          {
            tileArray(tx + x)(ty + y).occupied = false
            ty+=1
          }
          tx+=1;
        }
    tileArray(x)(y).place = null
  }
  def moveBuilding(x: Int, y: Int, to_x: Int, to_y: Int): Int = {
    if (insideBoudaries(x, y) && insideBoudaries(to_x, to_y)) {
      val building = tileArray(x)(y).place
      // if building is instance of Obstacle - don't move

      if (building != null) {
        if (!ObstacleManager.isObstalce(building.getName()))
        {
          val buildingName = building.getName()
          val size = BuildingManager.getSize(buildingName)
          removeBuilding(x, y, size)
          if (validatePlace(to_x, to_y, size) == GE.OK) {
            place(building, to_x, to_y, size)
          } else {
            place(building, x, y, size)
            GE.NotAbleToPlace
          }
        }  else GE.CantMoveObstacle
      } else GE.BuildingNotPlaced
    } else GE.GameEventOutOfBoundaries
  }
  def getBuilding(x: Int, y: Int): Buildable = tileArray(x)(y).place
  def validatePlace(x: Int, y: Int, size: (Int, Int)): Int = {
    if (((x < MapSettings.SIZE.x) && (y < MapSettings.SIZE.y)) && ((x >= 0) && (y >= 0)))
    {
      var tx = 0
      var ty = 0
      var check: Boolean = false;
      while (tx < size._1)
        {
          var ty = 0
          while (ty < size._2)
          {
            if (tileArray(tx + x)(ty + y).isOccupied()) check = true
            ty+=1
          }
          tx+=1;
        }
      if (check) GE.NotAbleToPlace else GE.OK
    } else GE.GameEventOutOfBoundaries
  }
  def insideBoudaries(x: Int, y: Int): Boolean = (((x < MapSettings.SIZE.x) && (y < MapSettings.SIZE.y)) && ((x >= 0) && (y >= 0)))
  def validateUse(x: Int, y: Int): Int = {
    if (tileArray(x)(y).isOccupied()) GE.OK
    else GE.BuildingNotPlaced
  }
  def reQueueAll(player: Player): Int = {
    var tx = 0
    var ty = 0
    var check: Boolean = false;
    while (tx < MapSettings.SIZE.x)
    {
      var ty = 0
      while (ty < MapSettings.SIZE.y)
      {
        if (tileArray(tx)(ty).place != null) tileArray(tx)(ty).place.reQueue(player)
        ty+=1
      }
      tx+=1;
    }
    GE.OK
  }
}
