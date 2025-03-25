package consoleAdmin

import zio.json._
import _root_.`<empty>`.App
import Console.{RESET, RED, BOLD, GREEN, WHITE}
import _root_.`<empty>`.PlayerManager
import _root_.gameClasses._
import java.time.Instant
import java.io.{BufferedWriter, FileWriter}
import _root_.`<empty>`.WebAppSignatureValidator
import _root_.`<empty>`.GameJSON
import _root_.`<empty>`.GE
import java.io.File

class Commands {
	var lastCommand: Option[() => String] = None

	def plus(a: Int, b: Int): String = {s"$a + $b = ${a+b}"}

	def ban(id: Long): String = { println(s"Banning user: '${id}'")
		try
			PlayerManager.banUser(id);
			s"${GREEN}Banned successfully!${RESET}"
		catch
			case _ => s"${RED}Problem occured while banning user '${id}'${RESET}"
	}
	def unban(id: Long): String = {
		println(s"Unbanning user: '${id}'")
		try
			PlayerManager.unbanUser(id);
			s"${GREEN}Unbanned successfully!${RESET}"
		catch
			case e: Exception => s"${RED}Problem occured while unbanning user '${id}'${RESET}"
	}

	def status(): String = {s"""Online: ${App.getOnline()}"""}

	def giftbooster(id: Long, b_type: String, time: Int, percent: Int): String = {
		lastCommand = Some(() => {
			try { PlayerManager.giftBooster(id, BoosterJSON(b_type, percent, time)); s"${GREEN}Success!$RESET"}
			catch
				case e: Exception => s"${RED}Could not gift Booster(type: $b_type, time: $time, percent: $percent) to user with id: $id.$RESET Error message: ${e.getMessage()}"
				case _ => s"${RED}Could not gift Booster(type: $b_type, time: $time, percent: $percent) to user with id: $id $RESET"
		})
		s"""You sure you want to gift Booster(type: $b_type, time: $time, percent: $percent) to user with id: $id?
[y()/n()]"""
	}

	def debug(): String = {
		App.debug = !App.debug
		s"Debug is ${if (App.debug) "turned on" else "turned off"}"
	}

	def y(): String = {
		lastCommand match
			case Some(value) => { lastCommand = None; value() }
			case None => "Nothing to confirm"
	}

	def n(): String = {
		lastCommand = None
		s"Cancelled!"
	}

	def testinitdata(): String = {
		val s = "query_id=AAHzJoppAAAAAPMmimntefo_&user=%7B%22id%22%3A1770661619%2C%22first_name%22%3A%22yidtdr%22%2C%22last_name%22%3A%22%22%2C%22username%22%3A%22yidtdr%22%2C%22language_code%22%3A%22en%22%2C%22allows_write_to_pm%22%3Atrue%7D&auth_date=1730399376&hash=5ef0deaa160a25c4ddc0b6841edba5d8a231cab169b961992abd9595a6d230be"
		s"${WebAppSignatureValidator.checkWebAppSignature(WebAppSignatureValidator.token, s)}"
	}

	def addmoney(id: Long, amount: Int) = {
		PlayerManager.addMoney(id, amount)
	}

	def getinfo(id: Long): String = {
		val game = PlayerManager.getInstance(id)
		PlayerManager.saveInstance(game)
		val snapshotsFolder = new File(s"snapshots/$id")
		val fileName = s"timestamp_${Instant.now().getEpochSecond()}.json"
		val filePath = s"snapshots/$id/$fileName"
		snapshotsFolder.mkdirs()
		game.lastOperation = GE.Connected
		val content = s"${GameJSON.fromGame(game).toJson}"
		val fileWriter = new BufferedWriter(new FileWriter(filePath))
		try {
			fileWriter.write(content)
			s"Successfully written to $filePath"
		} catch {
		case e: Exception =>
			s"An error occurred: ${e.getMessage}"
		} finally {
			fileWriter.close()
		}
	}

	def check() = {
		Plants.plants.foreach((el, info) => {
			val itemInf = (ItemManager.nameToOrderAmountLimits(el), ItemManager.nameToPrice(el), ItemManager.nameToTokenPrice(el))
			println(s"${el}: ${info}, min: ${itemInf._1._1} max: ${itemInf._1._2}")
		})
	}
}