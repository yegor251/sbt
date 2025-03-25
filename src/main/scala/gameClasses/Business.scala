package gameClasses

import zio.json._
import java.nio.file.{Files, Path}
import _root_.`<empty>`.ENVars
import _root_.`<empty>`.GE
import java.time.Instant
import scala.annotation.switch

case class BusinessINFO(price: Double, profitPerCoin: Double, maxCapacity: Double)
object BusinessINFO {
	implicit val decoder: JsonDecoder[BusinessINFO] = DeriveJsonDecoder.gen[BusinessINFO]
}
case class BusinessSettings(amount: Int, maxLevel: Int, levelDiffRatio: Double, businessDiffRatio: Double, firstBusiness: BusinessINFO)
object BusinessSettings {
	implicit val decoder: JsonDecoder[BusinessSettings] = DeriveJsonDecoder.gen[BusinessSettings]
}

class Business(id: Int, level: Int)
{
	var activateINFO: Option[(Instant, Int)] = None;
	val lvl = level;
	val ID = id;

	def activate(amount: Int): Int = {
		activateINFO match
			case Some(_i) => GE.BusinessAlreadyActive
			case None => (activateINFO = Some((Instant.now(), amount))); GE.OK
	}
}

object BusinessManager
{
	val path = ENVars.ASSETS.business;
	val bytes = Files.readAllBytes(path)
	val businessSettings = (new String(bytes, "UTF-8")).fromJson[BusinessSettings].left.map(new Exception(_)).getOrElse(null)
	if (businessSettings == null) println("[FATAL ERROR] Cannot read businessSettings")

	val businessArray = init();

	def init(): Array[Array[BusinessINFO]] = {
		var businessID = 0; var levelID = 0; var prev = businessSettings.firstBusiness;
		var businessArray = Array.fill(businessSettings.amount, businessSettings.maxLevel)(businessSettings.firstBusiness)
		while (businessID < businessSettings.amount) {
			if (businessID != 0) businessArray(businessID).update(0, BusinessINFO(prev.price * businessSettings.businessDiffRatio, prev.profitPerCoin * businessSettings.businessDiffRatio, prev.maxCapacity))
			prev = businessArray(businessID)(0);
			levelID = 1;
			while (levelID < businessSettings.maxLevel) {
				val temp = BusinessINFO(prev.price * businessSettings.levelDiffRatio, prev.profitPerCoin * businessSettings.levelDiffRatio, prev.maxCapacity * businessSettings.levelDiffRatio)
				businessArray(businessID).update(levelID, temp)
				prev = temp;
				levelID += 1;
			}
			businessID += 1;
		}

		businessArray
	}

	def debug() = {
		var a = 0; var b = 0;
		while (a < businessSettings.amount) {
			b = 0;
			while (b < businessSettings.maxLevel)
				{
					println(s"cap: ${businessArray(a)(b).maxCapacity} pr: ${businessArray(a)(b).price} ppc: ${businessArray(a)(b).profitPerCoin}")
					b += 1;
				}
			a+=1;
		}
	}

	def estimateProfit(business: Business): Int = (businessArray(business.ID)(business.lvl).profitPerCoin * business.activateINFO.getOrElse((null, 0))._2 * ENVars.GAME_SETTINGS.BUSINESSES.cycleTime).toInt

	def collect(player: Player, id: Int): Int = {
		if ((id >= 0) && (id < businessSettings.amount)) {
			player.Businesses.businesses(id) match
				case None => GE.BusinessDoesntExist;
				case Some(b) => { 
					b.activateINFO match
						case Some(tm) => {
							if (Instant.now().isAfter(tm._1.plusSeconds(ENVars.GAME_SETTINGS.BUSINESSES.cycleTime))) {
								player.Wallet.tokenBalance += estimateProfit(b)
								b.activateINFO = None;
								GE.OK
							} else GE.WorkNotReady
						}
						case None => GE.WorkNotStarted
				}
		} else GE.GameEventOutOfBoundaries
	}

	def upgradeBusiness(player: Player, id: Int): Int = {
		if ((id >= 0) && (id < businessSettings.amount)) {
			player.Businesses.businesses(id) match
				case None => GE.BusinessDoesntExist;
				case Some(b) => { 
					if (b.lvl < businessSettings.maxLevel) {
						if (b.activateINFO == None) {
							if (player.Wallet.tokenBalance >= businessArray(id)(b.lvl+1).price) {
								player.Wallet.tokenBalance -= businessArray(id)(b.lvl+1).price.longValue()
								player.Businesses.businesses.update(id, Some(Business(id, b.lvl+1)))
								GE.OK
							} else GE.NotEnoughMoney
						} else GE.BusinessAlreadyActive
					} else GE.GameEventOutOfBoundaries
				}
		} else GE.GameEventOutOfBoundaries
	}

	def purchaseBusiness(player: Player, id: Int): Int = {
		if ((id >= 0) && (id < businessSettings.amount)) {
			if (player.Wallet.tokenBalance >= businessArray(id)(0).price) {
				player.Businesses.businesses(id) match
					case None => { player.Businesses.businesses.update(id, Some(Business(id, 0))); player.Wallet.tokenBalance -= businessArray(id)(0).price.longValue(); GE.OK }
					case Some(b) => GE.BusinessAlreadyPurchased
			} else GE.NotEnoughMoney
		} else GE.GameEventOutOfBoundaries
	}

	def activate(player: Player, id: Int, amount: Int): Int = {
		if ((id >= 0) && (id < businessSettings.amount)) {
			if (player.Wallet.tokenBalance >= amount) {
				player.Businesses.businesses(id) match
					case None => GE.BusinessDoesntExist
					case Some(b) => { player.Wallet.tokenBalance -= amount; b.activate(amount) }
			} else GE.NotEnoughMoney
		} else GE.GameEventOutOfBoundaries
	}
}