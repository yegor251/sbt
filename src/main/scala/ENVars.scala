import com.typesafe.config.ConfigFactory
import java.nio.file.{Files, Path}

object ENVars {
	val readResult = ConfigFactory.parseFile(new java.io.File("configuration.conf"));
	val debug = readResult.getBoolean("debug");
	val botApiToken = readResult.getString("botToken")
	object DB
	{
		val dbHost = readResult.getString("DB.dbHost");
		val dbPort = readResult.getString("DB.dbPort");
		val dbName = readResult.getString("DB.dbName");
		val dbUser = readResult.getString("DB.dbUser");
		val dbPass = readResult.getString("DB.dbPass");
	}
	object GAME_SETTINGS
	{
		object ADMIN
		{
			val maxStrikes = 5;
		}

			/* MAP */
		object MAP
		{
			object SIZE
			{
				val x = 30;
				val y = 30;
			}

			val map = Path.of(s"resources/map/map.json")
		}
			/* SPIN */
		object SPIN
		{
			val timeToSpin = 28800;
		}

			/* ORDERS */
		object ORDERS
		{
			val ordersRerollTime = 60;
			val ordersAmountMin = 3;
			val ordersAmountMax = 5;
			val ordersRegenerationTime = 60;
		}

			/* TRANSACTIONS */
		object TRANSACTIONS
		{
			val maxPurchasedSlots = 3;
			val purchasedSlotsPrice = Array(1000, 10000, 100000);
		}

		object BUSINESSES
		{
			val cycleTime = 1296000;
		}

		object BUILDINGS
		{
			val building_bakery_per_building_k 	= 100
			val building_corral_per_building_k 	= 100
			val building_corral_per_animal_k 	= 1
			val building_garden_per_building_k 	= 1.2
		}
	}
	object ASSETS
	{
		val ambar = Path.of(s"resources/buildings/ambar.json")
		val business = Path.of(s"resources/business/business.json")
		val dealsFolder = "resources/deals"
		val itemlist = readResult.getStringList("ASSETS.ITEMS.list")
		var ITEMS: Map[String, Path] = Map.empty;
		itemlist.forEach((item) => {
			ITEMS = ITEMS.updated(item, Path.of(s"resources/items/${item}.json"));
		})

		val slist = readResult.getStringList("ASSETS.SEEDS.list")
		var SEEDS: Map[String, Path] = Map.empty;
		slist.forEach((item) => {
			SEEDS = SEEDS.updated(item, Path.of(s"resources/plants/${item}.json"));
		})

		val blist = readResult.getStringList("ASSETS.BUILDINGS.BAKERY.list")
		var BAKERIES: Map[String, Path] = Map.empty;
		blist.forEach((item) => {
			BAKERIES = BAKERIES.updated(item, Path.of(s"resources/buildings/${item}.json"));
		})

		val clist = readResult.getStringList("ASSETS.BUILDINGS.CORRAL.list")
		var CORRALS: Map[String, Path] = Map.empty;
		clist.forEach((item) => {
			CORRALS = CORRALS.updated(item, Path.of(s"resources/buildings/${item}.json"));
		})

		val bushlist = readResult.getStringList("ASSETS.BUILDINGS.BUSH.list")
		var BUSHES: Map[String, Path] = Map.empty;
		bushlist.forEach((item) => {
			BUSHES = BUSHES.updated(item, Path.of(s"resources/buildings/${item}.json"));
		})
		
		val garden = Path.of(s"resources/buildings/garden.json")
		
		val olist = readResult.getStringList("ASSETS.BUILDINGS.OBSTACLES.list")
		var OBSTACLES: Map[String, Path] = Map.empty;
		olist.forEach((item) => {
			OBSTACLES = OBSTACLES.updated(item, Path.of(s"resources/buildings/obstacles/${item}.json"));
		})
	}
}