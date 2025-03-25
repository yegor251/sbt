package gameClasses

import java.math.BigInteger
import _root_.`<empty>`.GE
import _root_.`<empty>`.ENVars
import _root_.`<empty>`.Game
import _root_.`<empty>`.DBInterface
import java.time.Instant

case class Withdraw(transaction_id: BigInteger, 
                var status: Short, 
                tg_id: BigInteger, 
                wallet: String,
                amount: BigInteger, 
                time_stamp: BigInteger) extends Serializable

object WithdrawManager
{
    def createWithdraw(game: Game, amount: Long, wallet: String): Int = { 
        if (amount <= game.player.Wallet.tokenBalance) {
            if (game.player.Wallet.tonBalance >= 100000000) {
                val withdraw = Withdraw(BigInteger.valueOf(Math.round(Math.random()*1000000000000000000L)), 0, BigInteger.valueOf(game.CLIENT_INFO.tgID), wallet, BigInteger.valueOf(amount), BigInteger.valueOf(Instant.now().getEpochSecond()))
                if (DBInterface.registerWithdraw(withdraw).getOrElse(-1) != -1) {
                    game.player.Wallet.tokenBalance -= amount;
                    game.player.Wallet.tonBalance -= 100000000
                    game.TRANSFERS_INFO.withdraws = game.TRANSFERS_INFO.withdraws :+ withdraw;
                    GE.OK
                }
                else GE.PaymentError
            }
            else GE.NotEnoughMoney
        }
        else GE.NotEnoughMoney
    }

    def getCurrentStatus(withdraw_ID: Long): Short =
    {
        val withdraw = DBInterface.checkWithdrawByInfo(withdraw_ID).getOrElse(null)
        if (withdraw != null)
        {
            withdraw(0).status
        }
        else 0
    }

    def checkAllWithdraws(game: Game): Int =
    {
        game.TRANSFERS_INFO.withdraws.map((w) => {
            w.status = getCurrentStatus(w.transaction_id.longValue())
        })
        GE.OK
    }
}