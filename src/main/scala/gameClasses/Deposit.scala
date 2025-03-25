package gameClasses

import java.math.BigInteger
import _root_.`<empty>`.GE
import _root_.`<empty>`.ENVars
import _root_.`<empty>`.Game
import _root_.`<empty>`.DBInterface

case class Deposit(transaction_id: BigInteger, var active: Boolean, tg_id: BigInteger, amount: BigInteger, time_stamp: BigInteger, jetton_signature: String, commentary: String) extends Serializable


object DepositManager
{
	def proceedToBalance(player: Player, amount: Long, jetton_signature: String) =
	{
		jetton_signature match
			case "TON" => player.Wallet.tonBalance += amount
			case "USD" => player.Wallet.usdtBalance += amount
			case "TFC" => player.Wallet.tokenBalance += amount
			case _ => // spasibo za donation ;)
	}
	def claim(game: Game, id: Int): Int =
	{
		val depositsAmount = game.TRANSFERS_INFO.deposits.length;
		if ((id >= 0) && (id < depositsAmount))
		{
			if (game.TRANSFERS_INFO.deposits(id).active)
			{
				game.TRANSFERS_INFO.deposits(id).active = false;
				val dep = game.TRANSFERS_INFO.deposits(id);
				proceedToBalance(game.player, dep.amount.longValue(), dep.jetton_signature)
				GE.OK
			}
			else
			{
				GE.DepositAlreadyActivated
			}
		}
		else
		{
			GE.GameEventOutOfBoundaries
		}
	}
	def check(game: Game): Int =
	{   
		var errorOccured = false;
		DBInterface.getDepositsByUserID(game.CLIENT_INFO.tgID).getOrElse(List.empty).foreach((dep) => {
			if (DBInterface.closeDepositByTransactionID(dep.transaction_id).getOrElse(-1) != -1) 
			{
				game.TRANSFERS_INFO.deposits = game.TRANSFERS_INFO.deposits :+ dep
			}
			else
			{
				errorOccured = true
			}
		})
		if (!errorOccured)
		{
			GE.OK
		}
		else
		{
			GE.PaymentError
		}
	}
}