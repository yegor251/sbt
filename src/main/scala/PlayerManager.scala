import App._
import gameClasses._

object PlayerManager {
	def getInstance(id: Long): Game = {
		val game = App.tokenToGame.get(id.toString()).getOrElse(null)
		if (game == null) BinaryDataManager.readGame(id.toString()) else game
	}

	def saveInstance(game: Game) = {
		BinaryDataManager.writeGame(game, game.CLIENT_INFO.tgID.toString())
	}

	def giftBooster(id: Long, booster: BoosterJSON) = {
		val game = getInstance(id)
		game.player.Deals.boosters = game.player.Deals.boosters.appended(booster);
		saveInstance(game)
	}

	def banUser(id: Long) = {
		val game = getInstance(id)
		game.onBan()
		saveInstance(game)
	}

	def unbanUser(id: Long) = {
		val game = getInstance(id)
		game.onUnban()
		saveInstance(game)
	}

	def addReferral(id: Long, ref_id: Long) = {
		val game = getInstance(ref_id)
		game.CLIENT_INFO.referrals = game.CLIENT_INFO.referrals.appended(id)
		game.player.Wallet.tokenBalance += 1000
		saveInstance(game)
	}

	def addMoney(id: Long, amount: Int) = {
		val game = getInstance(id)
		game.player.money += amount
		saveInstance(game)
	}
}