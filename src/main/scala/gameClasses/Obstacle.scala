package gameClasses

import zio._
import zio.json._
import java.nio.file.{Files, Path}
import java.time.{Instant, Duration}
import _root_.`<empty>`.ENVars
import _root_.`<empty>`.GE
import _root_.`<empty>`.Game
import gameClasses.BuildingManager.verifyPurchase

case class ObstacleJSON(name: String, removePrice: Option[Int], removeTokenPrice: Option[Int], sizex: Int, sizey: Int)

object ObstacleJSON
{
  implicit val decoder: JsonDecoder[ObstacleJSON] = DeriveJsonDecoder.gen[ObstacleJSON]
}

class Obstacle(obstacleType: String) extends Buildable
{
	val name = obstacleType

	def collect(): Int = GE.InternalServerLogicError
	def use(data: String): Int = GE.InternalServerLogicError
	def upgrade(): Int = GE.InternalServerLogicError
	def getName(): String = name
	def reQueue(player: Player): Unit = GE.InternalServerLogicError
}

object ObstacleManager
{
	var obstaclesINFO: Map[String, ObstacleJSON] = Map.empty

	for (name <- ENVars.ASSETS.OBSTACLES.keySet) {
		val bInfo = nameToLoadObstacle(name)
		obstaclesINFO = obstaclesINFO.updated(name, bInfo)
	}

	def nameToLoadObstacle(name: String): ObstacleJSON = loadObstacle(ENVars.ASSETS.OBSTACLES(name))

	def loadObstacle(path: Path): ObstacleJSON = {
		val bytes = Files.readAllBytes(path)
		val obstacle = (new String(bytes, "UTF-8")).fromJson[ObstacleJSON].left.map(new Exception(_))
		obstacle.getOrElse(null)
	}

	def obstacleByName(name: String): ObstacleJSON = obstaclesINFO(name)
	def isObstalce(name: String): Boolean = obstaclesINFO.isDefinedAt(name)

	def removeObstacle(game: Game, x: Int, y: Int): Int = {
		val building = game.world.getBuilding(x, y)
		if (building != null) {
			if (isObstalce(building.getName())) {
				val obstacle = obstacleByName(building.getName())
				val purchaseVerified = ((game.player.money >= obstacle.removePrice.getOrElse(0)) && (game.player.Wallet.tokenBalance >= obstacle.removeTokenPrice.getOrElse(0)))
				if (purchaseVerified) {
					game.player.money -= obstacle.removePrice.getOrElse(0)
					game.player.Wallet.tokenBalance -= obstacle.removeTokenPrice.getOrElse(0)

					game.world.tileArray(x)(y).place = null
					for (i <- x until (x + obstacle.sizex)) {
						for (j <- y until (y + obstacle.sizey)) {
							game.world.tileArray(i)(j).occupied = false
						}
					}

					GE.OK
				} else GE.NotEnoughMoney
			} else GE.SocketUnknown
		} else GE.BuildingNotPlaced
	}

	def ___PLACE_OBSTACLE_UNSAFE(game: Game, x: Int, y: Int, obstacle: String) = {
		game.world.tileArray(x)(y).place = new Obstacle(obstacle)

		for (i <- x until (x + obstacleByName(obstacle).sizex)) {
			for (j <- y until (y + obstacleByName(obstacle).sizey)) {
				game.world.tileArray(i)(j).occupied = true
			}
		}
	}
}