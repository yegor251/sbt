package gameClasses

import zio.json._
import java.nio.file.{Files, Path}
import _root_.`<empty>`.ENVars
import _root_.`<empty>`.GE

case class AmbarLevelsINFO(price: Int, capacityBonus: Int)
object AmbarLevelsINFO {
	implicit val decoder: JsonDecoder[AmbarLevelsINFO] = DeriveJsonDecoder.gen[AmbarLevelsINFO]
}

case class AmbarINFO(levels: Array[AmbarLevelsINFO])
object AmbarINFO {
	implicit val decoder: JsonDecoder[AmbarINFO] = DeriveJsonDecoder.gen[AmbarINFO]
}

object Ambar
{
	val path = ENVars.ASSETS.ambar;
	val bytes = Files.readAllBytes(path)
	val ambarINFO = (new String(bytes, "UTF-8")).fromJson[AmbarINFO].left.map(new Exception(_)).getOrElse(null)
	if (ambarINFO == null) println("[FATAL ERROR] Cannot read ambarINFO")
	val maxLevel = ambarINFO.levels.length;

	def upgradeInventory(player: Player): Int = {
		val level = player.Inventory.level
		val price = ambarINFO.levels(player.Inventory.level).price
		val capacityBonus = ambarINFO.levels(player.Inventory.level).capacityBonus
		if (level < maxLevel) {
			if (player.Wallet.tokenBalance >= price) {
				player.Wallet.tokenBalance -= price
				player.Inventory.enlargeCapacity(capacityBonus)
				player.Inventory.level += 1;
				GE.OK
			} else GE.NotEnoughMoney
		} else GE.BuildingAlreadyMaxxedUp
	}
}