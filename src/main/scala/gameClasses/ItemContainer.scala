package gameClasses

import scala.util.Random

class ItemsContainer extends Serializable{
	var items: Map[String, Int] = Map.empty;
	var itemsAmount: Int = 0;
	var capacity: Int = 70;
	var level: Int = 0;

	def getAmount(item: String): Option[Int] = {
		items.get(item)
	}

	def enlargeCapacity(amount: Int) = {
		capacity += amount
	}

	def checkAmount(amount: Int): Boolean = ((itemsAmount + amount) <= capacity)

	def checkMap(map: Map[String, Int]): Boolean = {
		var totalAmount = 0;
		map.foreach((name, amount) => totalAmount += amount)
		((totalAmount + itemsAmount) <= capacity)
	}

	def addMap(map: Map[String, Int]) = {
		var totalAmount = 0;
		map.foreach((name, amount) => totalAmount += amount)
		if (checkMap(map)) {
			map.foreach((n, a) => addAmountUnchecked(n, a))
		}
		itemsAmount += totalAmount
	}

	def addMapUnchecked(map: Map[String, Int]) = map.foreach((n, a) => addAmountUnchecked(n, a))

	def addAmountUnchecked(item: String, amount: Int) = {
		if (items.isDefinedAt(item)) {
			val newValue = items(item) + amount
				items = items.updated(item, newValue)
			}
		else {
			items = items.updated(item, amount)
		}
		itemsAmount += amount;
	}

	def addAmount(item: String, amount: Int) = {
		if (checkAmount(amount)) {
			if (items.isDefinedAt(item)) {
				val newValue = items(item) + amount
				items = items.updated(item, newValue)
			}
			else {
				items = items.updated(item, amount)
			}
			itemsAmount += amount;	
		}
	}

	def pullRandom(amount: Int): Array[String] = {
		if (items.isEmpty) return Array.empty[String]

		val keys = items.keys.toArray
		Array.fill(amount)(keys(Random.nextInt(keys.length)))
		}

	def pullRandomUnique(amount: Int = 1): Set[String] = {
		if (items.isEmpty) return Set.empty[String]

		val keys = items.keys.toArray
		Random.shuffle(keys).take(amount.min(keys.length)).toSet
	}
}