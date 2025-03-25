package gameClasses
import _root_.`<empty>`.GE
import _root_.`<empty>`.ENVars
import zio._
import zio.json._
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._



case class Deal(name: String, tonPrice: Option[Long],
				tokenPrice: Option[Long], usdtPrice: Option[Long],
				reward: Reward)
object Deal {
	implicit val decoder: JsonDecoder[Deal] = DeriveJsonDecoder.gen[Deal]
	implicit val encoder: JsonEncoder[Deal] = DeriveJsonEncoder.gen[Deal]
}

case class Reward(money: Option[Int], token: Option[Long], boosters: Option[Array[BoosterJSON]])
object Reward {
	implicit val decoder: JsonDecoder[Reward] = DeriveJsonDecoder.gen[Reward]
	implicit val encoder: JsonEncoder[Reward] = DeriveJsonEncoder.gen[Reward]
}

object Shop {
	@volatile var deals: Map[String, Deal] = loadDeals(ENVars.ASSETS.dealsFolder)

	private def loadDeals(folderPath: String): Map[String, Deal] = {
		val dirPath = Paths.get(folderPath)
		val files = Files.list(dirPath).iterator().asScala.toList.filter(_.toString.endsWith(".json"))

		val dealsList: List[Deal] = files.flatMap { file =>
		val content = new String(Files.readAllBytes(file), StandardCharsets.UTF_8)
		content.fromJson[Deal] match {
			case Right(deal) => Some(deal)
			case Left(error) =>
			println(s"Error parsing JSON from file $file: $error")
			None
		}}
		var dealsMap: Map[String, Deal] = Map.empty
		dealsList.foreach((deal) => {dealsMap = dealsMap.updated(deal.name, deal)})
		dealsMap
	}
	def reloadDeals() = {deals = loadDeals(ENVars.ASSETS.dealsFolder)}

	def validatePurchase(player: Player, dealName: String): Int = {
		if (!player.Deals.boughtDeals(dealName)){
			if (deals.isDefinedAt(dealName)) {
				val deal = deals(dealName)
				var validated = true
				if (player.Wallet.tokenBalance < deal.tokenPrice.getOrElse(0L)) validated = false
				if (player.Wallet.tonBalance < deal.tonPrice.getOrElse(0L)) validated = false
				if (player.Wallet.usdtBalance < deal.usdtPrice.getOrElse(0L)) validated = false
				if (validated) {
					player.Wallet.tokenBalance 	-= deal.tokenPrice.getOrElse(0L)
					player.Wallet.tonBalance 		-= deal.tonPrice.getOrElse(0L)
					player.Wallet.usdtBalance 	-= deal.usdtPrice.getOrElse(0L) 
					player.Deals.tokenSpent 	+= deal.tokenPrice.getOrElse(0L)
					player.Deals.tonSpent 		+= deal.tonPrice.getOrElse(0L)
					player.Deals.usdtSpent 		+= deal.usdtPrice.getOrElse(0L) 
					GE.OK
				} else GE.NotEnoughMoney
			} else GE.SocketWrongFormat
		} else GE.DealHasBeenAlreadyBought
	}

	def purchaseDeal(player: Player, dealName: String): Int = {
		val validationCode = validatePurchase(player, dealName);
		if (validationCode == GE.OK)
		{
				val deal = deals(dealName)
				val boostersArray: Array[BoosterJSON] = deal.reward.boosters.getOrElse(Array.empty)
				boostersArray.foreach((_b) => player.Deals.boosters = player.Deals.boosters.+:(_b))
				val money = deal.reward.money.getOrElse(0)
				player.money += money
				val tokens = deal.reward.token.getOrElse(0L)
				player.Wallet.tokenBalance += tokens
				player.Deals.boughtDeals = player.Deals.boughtDeals.incl(dealName)
		}
		validationCode
	}
	
	def buy(name: String, amount: Int, player: Player): Int = {
		val item = Plants.nameToPlant(name);
		if (item == null) GE.SocketWrongFormat
		else if (amount > 0) {
				if (player.money >= (amount * item.seed.price)) {
					if (player.Inventory.checkAmount(amount)) {
						player.money -= (amount * item.seed.price)
						player.Inventory.addAmount(item.name, amount)
						GE.OK
					} else GE.NoSpaceInInventory
			}
			else GE.NotEnoughMoney
		}
		else GE.InvalidAmount
	}
}
