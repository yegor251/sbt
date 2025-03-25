package gameClasses

import scala.util.Random
import _root_.`<empty>`.GE
import _root_.`<empty>`.ENVars
import java.time.Instant

class Order(orderItems: Map[String, Int], orderPrice: Int, timeStamp: Instant, orderTokenPrice: Int) extends Serializable
{
	val items = orderItems
	val price = orderPrice
	val tokenPrice = orderTokenPrice
	var completed = false
	val startTimeStamp = timeStamp
}

object OrderManager
{
	val random = Random
	def getOrdersAmount(player: Player): Int = ((Math.log(player.netWorth) / Math.log(5)).toInt + 1)
	def getKeySetSize(player: Player): Int = Math.min(8, Math.sqrt(2 * player.Inventory.items.size)).toInt
	def regenerate(player: Player): Int =
	{
		if (getOrdersAmount(player) > player.orders.size)
		{
			player.orders = player.orders.appended(generateOrder(player, Instant.now()))
		}
		GE.OK
	}
	def completeOrder(player: Player, orderID: Int): Int =
	{
		if (orderID < player.orders.size)
		{
			// if (!player.orders(orderID).completed)
			// {
				val itemBoosterPercentage = BoosterManager.getPercentage(player.ActiveBoosters.OrderItems)
				var ableToComplete = true;
				val order = player.orders(orderID)
				order.items.foreach(item => 
					{
						val amount = Math.round(item._2 * (1 - itemBoosterPercentage * 0.01)).toInt
						if (player.Inventory.getAmount(item._1).getOrElse(0) < amount){
								ableToComplete = false;
							}
					})
				if (order.startTimeStamp.isAfter(Instant.now()))
				{
					ableToComplete = false
				}
				if (ableToComplete) {
					player.orders(orderID).items.foreach(item => {
							val itemName = item._1;
							val amount = Math.round(item._2 * (1 - itemBoosterPercentage * 0.01)).toInt
							player.Inventory.addAmount(itemName, -amount)
						})
					val moneyBoosterPercentage = BoosterManager.getPercentage(player.ActiveBoosters.OrderMoney)
					val moneyReward = Math.round(order.price * (1 + moneyBoosterPercentage * 0.01)).toInt
					val tokenReward = Math.round(order.tokenPrice * (1 + moneyBoosterPercentage * 0.01)).toInt
					player.orders(orderID).completed = true;
					player.money += moneyReward;
					player.Stats.ordersCompleted += 1;
					player.netWorth += moneyReward;
					player.Wallet.tokenBalance += tokenReward;
					player.orders.update(orderID, generateOrder(player, Instant.now().plusSeconds(ENVars.GAME_SETTINGS.ORDERS.ordersRegenerationTime)))
					
					GE.OK
				}
				else
				{
					GE.OrderNotEnoughItemsToComplete
				}
		}
		else
		{
			GE.GameEventOutOfBoundaries
		}
	}
	def generateOrder(player: Player, timeStamp: Instant): Order =
	{
		try {
			val itemsAmount = random.nextInt(getKeySetSize(player)) + 1
			val keyset = player.Inventory.pullRandomUnique(itemsAmount)
			val priceModifier = (random.nextDouble() * 0.6 + 0.7)
			var moneyPrice = 0;
			var tokenPrice = 0;
			val itemsMap = keyset.map((el) => {
				val itemInf = (ItemManager.nameToOrderAmountLimits(el), ItemManager.nameToPrice(el), ItemManager.nameToTokenPrice(el))
				val amount = random.nextInt((itemInf._1._2 - itemInf._1._1).max(1)) + itemInf._1._1
				moneyPrice += itemInf._2 * amount
				tokenPrice += itemInf._3 * amount
				el -> amount
			}).toMap
			moneyPrice = (moneyPrice * priceModifier).toInt
			tokenPrice = (tokenPrice * priceModifier).toInt
			Order(itemsMap, moneyPrice, timeStamp, tokenPrice)
		}
		catch {
			case e: Throwable => {e.printStackTrace(); Order(Map("wheat" -> 3), 0, Instant.now(), 0)}
			case e: Exception => {e.printStackTrace(); Order(Map("wheat" -> 3), 0, Instant.now(), 0)}
			case _ => {println("hz"); Order(Map("wheat" -> 3), 0, Instant.now(), 0)}
		}
	}
	def rerollOrder(player: Player, orderID: Int): Int =
	{
		if ((orderID >= 0) && (orderID < player.orders.size))
		{
			if (player.orders(orderID).startTimeStamp.isBefore(Instant.now()))
			{
				if (!player.orders(orderID).completed)
				{
					player.orders.update(orderID, generateOrder(player, Instant.now().plusSeconds(ENVars.GAME_SETTINGS.ORDERS.ordersRerollTime)))

					GE.OK
				}
				else
				{
					GE.OrderAlreadyCompleted
				}
			} else GE.OrdersNotReady
		}
		else
		{
			GE.GameEventOutOfBoundaries
		}
	}
}